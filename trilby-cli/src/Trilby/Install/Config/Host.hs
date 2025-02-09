module Trilby.Install.Config.Host where

import Trilby.HNix
import Trilby.Install.Config.Edition
import Trilby.Install.Config.Release
import Trilby.Install.Config.User
import Prelude

data Keyboard = Keyboard
    { layout :: Text
    , variant :: Maybe Text
    }
    deriving stock (Generic, Show)

instance ToExpr Keyboard where
    toExpr Keyboard{..} =
        [nix|
        {
            layout = layout;
            variant = variant;
        }
        |]
            & canonicalSet

data Host = Host
    { hostname :: Text
    , edition :: Edition
    , release :: Release
    , keyboard :: Maybe Keyboard
    , locale :: Maybe Text
    , timezone :: Maybe Text
    , user :: User
    }
    deriving stock (Generic)

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
        }
        |]
            & _Fix
            . _NAbs
            . _2
            %~ canonicalSet
      where
        userModule = fromText @NExpr $ "../../users/" <> user.username
