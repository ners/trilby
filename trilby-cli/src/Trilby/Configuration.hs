module Trilby.Configuration where

import Data.Text qualified as Text
import Effectful.Reader.Static qualified as Reader
import Trilby.HNix
import Trilby.Host
import Trilby.Prelude

data Configuration = Configuration
    { name :: Text
    , host :: Host
    }
    deriving stock (Generic, Eq, Ord, Show)

hostname
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Eff es Text
hostname =
    Reader.ask >>= \case
        Localhost ->
            cached
                ( maybe (errorExit "hostname failed") pure
                    . (listToMaybe . Text.split (== '.') =<<)
                    <=< cmdOutTextFirstLine
                )
                ["hostname"]
        Host{..} -> pure hostname

fromHost
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Host
    -> Eff es Configuration
fromHost host = do
    name <- onHost host hostname
    pure Configuration{..}

reboot
    :: ( HasCallStack
       , IOE :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , Reader AppState :> es
       , Concurrent :> es
       )
    => Eff es ()
reboot = asRoot (withHost $ runProcess_ . proc) ["systemctl", "reboot"]

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
    System{kernel} <- hostSystem
    case action of
        ConfigBoot -> setProfile path
        ConfigSwitch -> setProfile path
        _ -> pure ()
    asRoot (withHost $ runProcess_ . proc) $ case kernel of
        Darwin -> pure . fromPath $ path </> $(mkRelFile "activate")
        Linux ->
            sconcat
                [ ["systemd-run"]
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
                , [fromPath $ path </> $(mkRelFile "bin/switch-to-configuration"), ishow action]
                ]

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

buildConfiguration
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Configuration
    -> Eff es (Path Abs Dir)
buildConfiguration Configuration{..} = do
    System{kernel} <- onHost host hostSystem
    [out] <-
        onHost Localhost
            . nixBuild
            . Flake
            $ FlakeRef
                { url = fromPath $ trilbyHome rootDir
                , output =
                    case kernel of
                        Darwin -> ["darwinConfigurations", name, "system"]
                        Linux -> ["nixosConfigurations", name, "config", "system", "build", "toplevel"]
                }
    pure out

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
buildConfigurations (configuration :| []) = pure . (configuration,) <$> buildConfiguration configuration
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
