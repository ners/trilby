module Trilby.Config where

import GHC.Generics (Generic)
import Trilby.Config.Host (Host)
import Trilby.Disko (Disko)
import Prelude

data InstallConfig = InstallConfig
    { host :: Host
    , disko :: Disko
    }
    deriving stock (Generic, Show)
