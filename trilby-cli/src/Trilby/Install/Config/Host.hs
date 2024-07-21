module Trilby.Install.Config.Host where

import Trilby.HNix (canonicalSet)
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
    , keyboard :: Keyboard
    , locale :: Text
    , timezone :: Text
    , user :: User
    }
    deriving stock (Generic)

instance ToExpr Host where
    toExpr Host{..} =
        [nix|
        { lib, ... }:

        {
          imports = [
            (import userModule { inherit lib; })
          ];
          networking.hostName = hostname;
          time.timeZone = timezone;
          i18n = rec {
            defaultLocale = locale;
            extraLocaleSettings.LC_ALL = defaultLocale;
          };
          services.xserver.xkb = keyboard;
        }
        |]
      where
        userModule :: NExpr
        userModule = fromText $ "../../users/" <> user.username
