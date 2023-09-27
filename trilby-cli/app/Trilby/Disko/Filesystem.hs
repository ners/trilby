module Trilby.Disko.Filesystem where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data Format
    = Btrfs
    | Ext4
    | Fat32
    | XFS
    deriving stock (Generic, Show, Eq)

data Filesystem = Filesystem
    { format :: Format
    , mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)
