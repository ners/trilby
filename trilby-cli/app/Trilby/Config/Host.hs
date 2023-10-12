module Trilby.Config.Host where

import Data.Text (Text)
import GHC.Generics (Generic)
import Nix (NExpr)
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.Config.Channel
import Trilby.Config.Edition
import Trilby.Config.User
import Trilby.Util
import Prelude

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
