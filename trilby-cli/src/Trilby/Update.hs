{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Update (update) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Trilby.HNix (FileOrFlake (..), FlakeRef (..), nixBuild, writeNixFile)
import Trilby.Update.Options
import Turtle (readlink, (</>))
import Trilby.Host
import Prelude

update :: UpdateOpts Maybe -> App ()
update (askOpts -> opts) = do
    whenM opts.flakeUpdate . inDir "/etc/trilby" $ rawCmd_ ["nix", "flake", "update", "--accept-flake-config"]
    hostname <- Text.strip <$> cmd ["hostnamectl", "hostname"]
    let canonicalHost :: Host -> Host
        canonicalHost Localhost = Localhost
        canonicalHost host@Host{hostname = h}
            | h `elem` hostname :| ["localhost", "127.0.0.1", "::1"] = Localhost
            | otherwise = host
    let configuration :: Host -> Configuration
        configuration =
            canonicalHost >>> \case
                Localhost -> Configuration{name = hostname, host = Localhost}
                host@Host{hostname} -> Configuration{name = hostname, host}
    configurations <- configuration <$$> opts.hosts <&> NonEmpty.nubOrd
    configurationResults <- buildConfigurations configurations
    for_ configurationResults $ \(Configuration{..}, resultPath) -> do
        unless (host == Localhost) $ copyClosure host resultPath
        ssh host rawCmd_ ["unbuffer", "nvd", "diff", "/run/current-system", fromString resultPath]
        unless (host == Localhost && length configurationResults == 1) $ $(logWarn) $ "Choosing action for host " <> ishow host
        let perform = switchToConfiguration host resultPath
        opts.action >>= \case
            Switch -> perform ConfigSwitch
            Boot{..} -> do
                perform ConfigBoot
                whenM reboot $ ssh host cmd_ ["systemctl", "reboot"]
            Test -> perform ConfigTest
            NoAction -> pure ()

data Configuration = Configuration {name :: Text, host :: Host}
    deriving stock (Generic, Eq, Ord)

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

copyClosure :: Host -> FilePath -> App ()
copyClosure Localhost _ = pure ()
copyClosure host@Host{} path = cmd_ ["nix-copy-closure", "--gzip", "--to", ishow host, fromString path]

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
