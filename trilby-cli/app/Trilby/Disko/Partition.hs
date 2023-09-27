{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Disko.Partition where

import Prelude
import GHC.Generics (Generic)
import Data.Text (Text)
import Trilby.Disko.Filesystem

data Size = GiB Int | Whole
    deriving stock (Generic, Eq, Show)

data Subvolume = Subvolume
    { mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)

data Partition
    = EfiPartition
        { size :: Size
        , filesystem :: Filesystem
        }
    | LuksPartition
        { size :: Size
        , partitions :: [Partition]
        }
    | BtrfsPartition
        { size :: Size
        , subvolumes :: [Subvolume]
        }
    | DataPartition
        { size :: Size
        , filesystem :: Filesystem
        }
    deriving stock (Generic, Show, Eq)
