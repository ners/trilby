module Trilby.Update (update) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), FlakeRef (..), copyClosure, nixBuild, writeNixFile)
import Trilby.Host
import Trilby.Update.Options
import Turtle qualified
import Prelude

update :: (HasCallStack) => UpdateOpts Maybe -> App ()
update (askOpts -> opts) = do
    whenM opts.flakeUpdate do
        trilbyPath <- canonicalizePath $ trilbyHome rootDir
        flakes <- Turtle.sort $ Turtle.parent <$> Turtle.find (Turtle.suffix "/flake.lock") (toFilePath trilbyPath)
        mapM_ updateFlake flakes
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    buildConfigurations configurations >>= mapM_ \(Configuration{..}, resultPath) -> do
        traceShowM $ resultPath </> $(mkRelFile "bin/switch-to-configuration")
        copyClosure host resultPath
        ssh host rawCmd_ ["unbuffer", "nvd", "diff", "/run/current-system", fromPath resultPath]
        unless (host == Localhost && length configurations == 1) . logWarn $ "Choosing action for host " <> ishow host
        let perform = switchToConfiguration host resultPath
        opts.action >>= \case
            Switch -> perform ConfigSwitch
            Boot{..} -> do
                perform ConfigBoot
                Trilby.Host.reboot reboot host
            Test -> perform ConfigTest
            NoAction -> pure ()

updateFlake :: (HasCallStack) => FilePath -> App ()
updateFlake path = rawCmd_ ["nix", "flake", "update", "--accept-flake-config", "--flake", fromString path]

buildConfiguration :: (HasCallStack) => Configuration -> App (Configuration, Path Abs Dir)
buildConfiguration c = (c,) <$> nixBuild f parseAbsDir
  where
    output = ["nixosConfigurations", c.name, "config", "system", "build", "toplevel"]
    f = Flake FlakeRef{url = fromPath $ trilbyHome rootDir, output}

{- | We wish to build multiple configurations, but avoid evaluating Trilby and Nixpkgs multiple times.
To this end we write a single derivation that depends on each of the configurations we wish to build.
The resulting output path contains symlinks for each configuration by name.
-}
buildConfigurations :: (HasCallStack) => NonEmpty Configuration -> App (NonEmpty (Configuration, Path Abs Dir))
buildConfigurations (configuration :| []) = pure <$> buildConfiguration configuration
buildConfigurations configurations = withTempFile $(mkRelFile "update.nix") $ \tmpFile -> do
    let configurationNames = configurations <&> (.name)
    writeNixFile
        tmpFile
        [nix|
         { local ? builtins.getFlake "/etc/trilby"
         , trilby ? local.inputs.trilby
         , pkgs ? trilby.inputs.nixpkgs.outputs.legacyPackages.${builtins.currentSystem}
         }:
         pkgs.linkFarm "trilby-update"
             (builtins.map
               (name: {
                 inherit name;
                 path = local.outputs.nixosConfigurations.${name}.config.system.build.toplevel;
               })
               configurationNames
             )
         |]
    resultPath <- nixBuild (File $ Abs tmpFile) parseAbsDir
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

switchToConfiguration :: (HasCallStack) => Host -> Path Abs Dir -> ConfigAction -> App ()
switchToConfiguration host path action = do
    case action of
        ConfigBoot -> setProfile host path
        ConfigSwitch -> setProfile host path
        _ -> pure ()
    ssh host rawCmd_ . sconcat $
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

setProfile :: (HasCallStack) => Host -> Path Abs Dir -> App ()
setProfile host path =
    ssh host rawCmd_ . sconcat $
        [ ["sudo"]
        , ["nix-env"]
        , ["--profile", "/nix/var/nix/profiles/system"]
        , ["--set", fromPath path]
        ]
