module Trilby.Config where

import GHC.Generics (Generic)
import Trilby.Config.Edition
import Trilby.Config.User
import Trilby.Disko (Disko, defaultDisko)
import Prelude
import Trilby.Config.Host

data InstallConfig = InstallConfig
    { host :: Host
    , disko :: Disko
    }
    deriving stock (Generic, Show)

defaultTrilbyConfig :: InstallConfig
defaultTrilbyConfig =
    InstallConfig
        { host =
            Host
                { hostname = "trilby"
                , channel = "unstable"
                , keyboardLayout = "us"
                , timezone = "Europe/Zurich"
                , edition = Workstation
                , user =
                    User
                        { username = "trilby"
                        , password = "trilby"
                        }
                }
        , disko = defaultDisko
        }
