module Trilby.Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Trilby.Disko (Disko, defaultDisko)
import Prelude

type Username = Text

type Password = Text

data Edition = Workstation | Server
    deriving stock (Generic, Show, Read, Eq)

data User = User
    { username :: Text
    , password :: Text
    }
    deriving stock (Generic, Show)

data TrilbyConfig = TrilbyConfig
    { keyboardLayout :: Text
    , timezone :: Text
    , edition :: Edition
    , user :: User
    }
    deriving stock (Generic, Show)

data InstallConfig = InstallConfig
    { trilby :: TrilbyConfig
    , disko :: Disko
    }
    deriving stock (Generic, Show)

defaultTrilbyConfig :: InstallConfig
defaultTrilbyConfig =
    InstallConfig
        { trilby =
            TrilbyConfig
                { keyboardLayout = "us"
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
