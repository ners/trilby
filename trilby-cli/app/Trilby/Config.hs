module Trilby.Config where

import Data.Default (Default (def))
import GHC.Generics (Generic)
import Trilby.Config.Host (Host)
import Trilby.Disko (Disko)
import Prelude

data InstallConfig = InstallConfig
    { host :: Host
    , disko :: Disko
    }
    deriving stock (Generic, Show)

instance Default InstallConfig where
    def = InstallConfig{host = def, disko = def}
