{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Disko.Partition where

import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.Disko.Filesystem (Filesystem)
import Prelude

data Size = GiB Int | Whole
    deriving stock (Generic, Eq, Show)

instance ToExpr Size where
    toExpr (GiB gib) = toExpr $ show gib <> "G"
    toExpr Whole = toExpr @String "100%"

data Subvolume = Subvolume
    { mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Subvolume where
    toExpr Subvolume{..} =
        [nix|
        {
            mountpoint = mountpoint;
            mountoptions = mountoptions;
        }
        |]

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

instance ToExpr Partition where
    toExpr EfiPartition{..} =
        [nix|
        {
            size = size;
            type = "EF00";
            content = filesystem;
        }
        |]
    toExpr LuksPartition{..} =
        [nix|
        {
            size = size;
            type = "luks";
            partitions = partitions;
        }
        |]
    toExpr BtrfsPartition{..} =
        [nix|
        {
            size = size;
            type = "btrfs";
            subvolumes = subvolumes;
        }
        |]
    toExpr DataPartition{..} =
        [nix|
        {
            size = size;
            type = "8300";
            content = filesystem;
        }
        |]
