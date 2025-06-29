module Trilby.Update (update) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Path.IO (listDirRecur)
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), copyClosure, nixBuild, writeNixFile)
import Trilby.Host
import Trilby.System
import Trilby.Update.Options
import Prelude

update :: UpdateOpts Maybe -> App ()
update (askOpts -> opts) = do
    trilbyPath <- canonicalizePath $ trilbyHome rootDir
    whenM opts.flakeUpdate do
        flakes <- fmap parent . filter (("flake.lock" ==) . toFilePath . filename) . snd <$> listDirRecur trilbyPath
        mapM_ updateFlake flakes
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    hostSystem Localhost >>= \case
        System{kernel = Linux} -> do
            buildConfigurations configurations >>= mapM_ \(Configuration{..}, resultPath) -> do
                copyClosure host resultPath
                ssh host runProcess'_ ["unbuffer", "nvd", "diff", "/run/current-system", fromPath resultPath]
                unless (host == Localhost && length configurations == 1) . logWarn $ "Choosing action for host " <> ishow host
                let perform = switchToConfiguration host resultPath
                opts.action >>= \case
                    Switch -> perform ConfigSwitch
                    Boot{..} -> do
                        perform ConfigBoot
                        Trilby.Host.reboot reboot host
                    Test -> perform ConfigTest
                    NoAction -> pure ()
        System{kernel = Darwin} -> updateDarwin opts trilbyPath

updateFlake :: Path b Dir -> App ()
updateFlake path = runProcess'_ ["nix", "flake", "update", "--accept-flake-config", "--flake", fromPath path]

{- | We wish to build multiple configurations, but avoid evaluating Trilby and Nixpkgs multiple times.
To this end we write a single derivation that depends on each of the configurations we wish to build.
The resulting output path contains symlinks for each configuration by name.
-}
buildConfigurations :: NonEmpty Configuration -> App (NonEmpty (Configuration, Path Abs Dir))
buildConfigurations (configuration :| []) = (configuration,) <$$> NonEmpty.fromList <$> nixBuild f
  where
    f =
        Flake
            FlakeRef
                { url = fromPath $ trilbyHome rootDir
                , output = ["nixosConfigurations", configuration.name, "config", "system", "build", "toplevel"]
                }
buildConfigurations configurations = withTempFile $(mkRelFile "update.nix") $ \tmpFile -> do
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
    flip genM configurations $ getSymlinkTarget parseAbsDir . Abs . (resultPath </>) <=< parseRelFile . fromText . (.name)

data ConfigAction
    = ConfigBoot
    | ConfigSwitch
    | ConfigTest
    deriving stock (Generic, Eq)

instance Show ConfigAction where
    show ConfigBoot = "boot"
    show ConfigSwitch = "switch"
    show ConfigTest = "test"

switchToConfiguration :: Host -> Path Abs Dir -> ConfigAction -> App ()
switchToConfiguration host path action = do
    case action of
        ConfigBoot -> setProfile host path
        ConfigSwitch -> setProfile host path
        _ -> pure ()
    ssh host runProcess'_ . sconcat $
        [ ["sudo"]
        , ["systemd-run"]
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

setProfile :: Host -> Path Abs t -> App ()
setProfile host path =
    ssh host runProcess'_ . sconcat $
        [ ["sudo"]
        , ["nix-env"]
        , ["--profile", "/nix/var/nix/profiles/system"]
        , ["--set", fromPath path]
        ]

updateDarwin :: UpdateOpts App -> Path Abs Dir -> App ()
updateDarwin opts trilbyDir = inDir trilbyDir do
    runProcess'_ ["darwin-rebuild", "build", "--flake", fromPath trilbyDir]
    let result = $(mkRelDir "./result")
    runProcess'_ ["unbuffer", "nvd", "diff", "/run/current-system", fromPath result]
    systemConfig <- canonicalizePath result
    let perform action = do
            case action of
                ConfigSwitch -> setProfile Localhost systemConfig
                _ -> pure ()
            runProcess'_ [fromPath $ result </> $(mkRelFile "activate-user")]
            asRoot runProcess'_ [fromPath $ result </> $(mkRelFile "activate")]
    opts.action >>= \case
        Switch -> perform ConfigSwitch
        Test -> perform ConfigTest
        Boot{} -> errorExit "Boot is not supported on Darwin"
        NoAction -> pure ()
