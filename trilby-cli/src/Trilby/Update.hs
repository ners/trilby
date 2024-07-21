module Trilby.Update (update) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), FlakeRef (..), copyClosure, nixBuild, writeNixFile)
import Trilby.Host
import Trilby.Update.Options
import Turtle (parent, readlink, (</>))
import Turtle qualified
import UnliftIO.Directory (canonicalizePath)
import Prelude

update :: UpdateOpts Maybe -> App ()
update (askOpts -> opts) = do
    whenM opts.flakeUpdate do
        trilbyHome <- canonicalizePath "/etc/trilby"
        flakes <- Turtle.sort $ parent <$> Turtle.find (Turtle.suffix "/flake.lock") trilbyHome
        mapM_ updateFlake flakes
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    configurationResults <- buildConfigurations configurations
    for_ configurationResults $ \(Configuration{..}, resultPath) -> do
        copyClosure host resultPath
        ssh host rawCmd_ ["unbuffer", "nvd", "diff", "/run/current-system", fromString resultPath]
        unless (host == Localhost && length configurationResults == 1) . logWarn $ "Choosing action for host " <> ishow host
        let perform = switchToConfiguration host resultPath
        opts.action >>= \case
            Switch -> perform ConfigSwitch
            Boot{..} -> do
                perform ConfigBoot
                whenM reboot $ ssh host cmd_ ["systemctl", "reboot"]
            Test -> perform ConfigTest
            NoAction -> pure ()

updateFlake :: FilePath -> App ()
updateFlake path = rawCmd_ ["nix", "flake", "update", "--accept-flake-config", "--flake", fromString path]

buildConfiguration :: Configuration -> App (Configuration, FilePath)
buildConfiguration c = (c,) <$> nixBuild f
  where
    output = ["nixosConfigurations", c.name, "config", "system", "build", "toplevel"]
    f = Flake FlakeRef{url = "/etc/trilby", output}

{- | We wish to build multiple configurations, but avoid evaluating Trilby and Nixpkgs multiple times.
To this end we write a single derivation that depends on each of the configurations we wish to build.
The resulting output path contains symlinks for each configuration by name.
-}
buildConfigurations :: NonEmpty Configuration -> App (NonEmpty (Configuration, FilePath))
buildConfigurations (configuration :| []) = pure <$> buildConfiguration configuration
buildConfigurations configurations = withSystemTempFile "trilby-update-.nix" $ \tmpFile handle -> do
    hClose handle
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
    resultPath <- nixBuild $ File tmpFile
    forM configurations $ \c -> (c,) <$> readlink (resultPath </> fromText c.name)

data ConfigAction
    = ConfigBoot
    | ConfigSwitch
    | ConfigTest
    deriving stock (Generic, Eq)

instance Show ConfigAction where
    show ConfigBoot = "boot"
    show ConfigSwitch = "switch"
    show ConfigTest = "test"

switchToConfiguration :: Host -> FilePath -> ConfigAction -> App ()
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
        , [fromString activationScript, ishow action]
        ]
  where
    activationScript = path <> "/bin/switch-to-configuration"

setProfile :: Host -> FilePath -> App ()
setProfile host path =
    ssh host rawCmd_ . sconcat $
        [ ["sudo"]
        , ["nix-env"]
        , ["--profile", "/nix/var/nix/profiles/system"]
        , ["--set", fromString path]
        ]
