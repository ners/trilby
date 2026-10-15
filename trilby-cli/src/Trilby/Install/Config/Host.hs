module Trilby.Install.Config.Host where

import Trilby.HNix
import Trilby.Install.Config.Edition
import Trilby.Install.Config.Keyboard
import Trilby.Install.Config.Release
import Trilby.Install.Config.User
import Trilby.Prelude

data Host = Host
    { hostname :: Text
    , edition :: Edition
    , release :: Release
    , keyboard :: Maybe Keyboard
    , locale :: Maybe Text
    , timezone :: Maybe Text
    , stateVersion :: Maybe Text
    , bios :: Bool
    , user :: User
    }
    deriving stock (Generic)

systemModule :: Edition -> String -> NExpr
systemModule edition platform =
    [nix|
    { inputs, lib, ... }:
    lib.trilbySystem {
      inherit inputs;
      trilby = {
        edition = edition;
        buildPlatform = platform;
        hostPlatform = platform;
      };
      modules = lib.findModulesList ./.;
    }
    |]

instance ToExpr Host where
    toExpr Host{..} =
        [nix|
        { lib, ... }:

        {
          imports = [ userModule ];

          networking.hostName = hostname;

          services.xserver.xkb = keyboard;

          i18n.defaultLocale = locale;
          i18n.extraLocaleSettings.LC_ALL = locale;

          time.timeZone = timezone;

          system.stateVersion = stateVersion;

          boot.loader.grub.enable = grub;
          boot.loader.systemd-boot.enable = systemdBoot;
          boot.loader.efi.canTouchEfiVariables = systemdBoot;
        }
        |]
            & _Fix
            . _NAbs
            . _2
            %~ canonicalSet
      where
        grub = if bios then Just True else Nothing
        systemdBoot = if bios then Just False else Nothing
        userModule = fromText @NExpr $ "../../users/" <> user.username
