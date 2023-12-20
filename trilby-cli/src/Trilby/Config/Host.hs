module Trilby.Config.Host where

import Internal.Prelude
import Trilby.Config.Channel
import Trilby.Config.Edition
import Trilby.Config.User

data Host = Host
    { hostname :: Text
    , edition :: Edition
    , channel :: Channel
    , keyboardLayout :: Text
    , timezone :: Text
    , user :: User
    }
    deriving stock (Generic, Show)

instance ToExpr Host where
    toExpr Host{..} =
        [nix|
            { lib, ... }:

            lib.trilbySystem {
              trilby = {
                edition = edition;
                channel = channel;
              };
              modules = [
                {
                  networking.hostName = hostname;
                  time.timeZone = timezone;
                }
                ./hardware-configuration.nix
                ./disko.nix
                (import userModule { inherit lib; })
              ];
            }
        |]
      where
        userModule :: NExpr
        userModule = fromText $ "../../users/" <> user.username
