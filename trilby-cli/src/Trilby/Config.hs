module Trilby.Config where

import Internal.Prelude
import Trilby.Config.Host (Host)
import Trilby.Disko (Disko)

data InstallConfig = InstallConfig
    { host :: Host
    , disko :: Disko
    }
    deriving stock (Generic, Show)
