module Trilby.Update (update) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Effectful.Reader.Static qualified as Reader
import System.FilePath.Glob (globDir1)
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), copyClosure, nixBuild, writeNixFile)
import Trilby.Host
import Trilby.Prelude
import Trilby.Update.Options

update
    :: forall es
     . ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => UpdateOpts Maybe
    -> Eff es ()
update (askOpts -> opts) = onHost Localhost do
    trilbyPath <- canonicalizePath $ trilbyHome rootDir
    whenM opts.flakeUpdate do
        isGit <- doesDirExist $ trilbyPath </> $(mkRelDir ".git")
        let filterGitTracked
                | isGit = filterM $ isGitTracked trilbyPath
                | otherwise = pure
        flakes <- liftIO $ mapM parseAbsFile =<< globDir1 "**/flake.lock" (toFilePath trilbyPath)
        mapM_ updateFlake =<< filterGitTracked flakes
    localSystem <- onHost Localhost hostSystem
    opts.hosts >>= mapM_ \host -> onHost host do
        system <- hostSystem
        unless (system == localSystem) $ errorExit "Cross building is currently not supported"
        case system.kernel of
            Darwin -> inject $ updateDarwin opts trilbyPath
            Linux -> inject $ updateLinux opts

updateFlake
    :: (HasCallStack, TypedProcess :> es, Log :> es)
    => Path b File
    -> Eff es ()
updateFlake path =
    runProcess_
        . proc
        $ ["nix", "flake", "update", "--accept-flake-config", "--flake", fromPath $ parent path]

-- | We wish to build multiple configurations, but avoid evaluating Trilby and Nixpkgs multiple times.
-- To this end we write a single derivation that depends on each of the configurations we wish to build.
-- The resulting output path contains symlinks for each configuration by name.
buildConfigurations
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => NonEmpty Configuration
    -> Eff es (NonEmpty (Configuration, Path Abs Dir))
buildConfigurations (configuration :| []) = (configuration,) <$$> NonEmpty.fromList <$> nixBuild f
  where
    f =
        Flake
            FlakeRef
                { url = fromPath $ trilbyHome rootDir
                , output = ["nixosConfigurations", configuration.name, "config", "system", "build", "toplevel"]
                }
buildConfigurations configurations = onHost Localhost . withTempFile $(mkRelFile "update.nix") $ \tmpFile -> do
    let configurationNames = configurations <&> (.name)
    writeNixFile
        tmpFile
        [nix|
          { local ? builtins.getFlake "/etc/trilby"
          , trilby ? local.inputs.trilby
          , lib ? trilby.lib
          , pkgs ? trilby.inputs.nixpkgs.outputs.legacyPackages.${builtins.currentSystem}
          }:
            lib.pipe configurationNames [
            (builtins.map (name: {
              inherit name;
              path = local.outputs.nixosConfigurations.${name}.config.system.build.toplevel;
            }))
            (pkgs.linkFarm "trilby-update")
          ]
        |]
    [resultPath] <- nixBuild (File $ Abs tmpFile)
    flip genM configurations
        $ getSymlinkTarget parseAbsDir
        . Abs
        . (resultPath </>)
        <=< parseRelFile
        . fromText
        . (.name)

data ConfigAction
    = ConfigBoot
    | ConfigSwitch
    | ConfigTest
    deriving stock (Generic, Eq)

instance Show ConfigAction where
    show ConfigBoot = "boot"
    show ConfigSwitch = "switch"
    show ConfigTest = "test"

switchToConfiguration
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Path Abs Dir
    -> ConfigAction
    -> Eff es ()
switchToConfiguration path action = do
    case action of
        ConfigBoot -> setProfile path
        ConfigSwitch -> setProfile path
        _ -> pure ()
    asRoot (withHost $ runProcess_ . proc)
        . sconcat
        $ [ ["systemd-run"]
          , ["-E", "LOCALE_ARCHIVE"]
          , ["-E", "NIXOS_INSTALL_BOOTLOADER=1"]
          , ["--collect"]
          , ["--no-ask-password"]
          , ["--pty"]
          , ["--quiet"]
          , ["--same-dir"]
          , ["--service-type=exec"]
          , ["--unit=trilby-switch-to-configuration"]
          , ["--wait"]
          , [fromPath activationScript, ishow action]
          ]
  where
    activationScript = path </> $(mkRelFile "bin/switch-to-configuration")

setProfile
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Path Abs t
    -> Eff es ()
setProfile path =
    asRoot (withHost $ runProcess_ . proc)
        . sconcat
        $ [ ["nix-env"]
          , ["--profile", "/nix/var/nix/profiles/system"]
          , ["--set", fromPath path]
          ]

updateLinux
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => UpdateOpts (Eff es)
    -> Eff es ()
updateLinux opts = do
    host <- Reader.ask
    configuration <- Configuration.fromHost host
    buildConfigurations (pure configuration) >>= mapM_ \(Configuration{..}, resultPath) -> do
        copyClosure resultPath
        withHost
            (runProcess_ . proc)
            ["nvd", "--color", "always", "diff", "/run/current-system", fromPath resultPath]
        unless (host == Localhost) . logAttention_ $ "Choosing action for host " <> ishow host
        let perform = switchToConfiguration resultPath
        opts.action >>= \case
            Switch -> perform ConfigSwitch
            Boot{..} -> do
                perform ConfigBoot
                whenM reboot Configuration.reboot
            Test -> perform ConfigTest
            NoAction -> pure ()

updateDarwin
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => UpdateOpts (Eff es)
    -> Path Abs Dir
    -> Eff es ()
updateDarwin opts trilbyDir = onHost Localhost . inDir trilbyDir $ do
    cmd_ ["darwin-rebuild", "build", "--flake", fromPath trilbyDir]
    let result = $(mkRelDir "./result")
    cmd_ ["nvd", "--color", "always", "diff", "/run/current-system", fromPath result]
    systemConfig <- canonicalizePath result
    let perform action = do
            case action of
                ConfigSwitch -> setProfile systemConfig
                _ -> pure ()
            cmd_ [fromPath $ result </> $(mkRelFile "activate-user")]
            asRoot cmd_ [fromPath $ result </> $(mkRelFile "activate")]
    inject opts.action >>= \case
        Switch -> perform ConfigSwitch
        Test -> perform ConfigTest
        Boot{} -> errorExit "Boot is not supported on Darwin"
        NoAction -> pure ()
