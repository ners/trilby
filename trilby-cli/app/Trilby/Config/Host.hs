module Trilby.Config.Host where

import Data.Text (Text)
import GHC.Generics (Generic)
import Nix (NExpr)
import Nix.TH (nix)
import Trilby.Config.Edition (Edition)
import Trilby.Config.User
import Prelude

data Host = Host
    { hostname :: Text
    , channel :: Text
    , keyboardLayout :: Text
    , timezone :: Text
    , edition :: Edition
    , user :: User
    }
    deriving stock (Generic, Show)

host :: Host -> NExpr
host Host{..} =
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
            (importStmt userModule { inherit lib; })
          ];
        }
    |]
  where
    importStmt = "import" :: NExpr
    userModule = "../../users/" <> user.username
