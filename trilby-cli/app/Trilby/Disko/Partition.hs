{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Disko.Partition where

import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.Disko.Filesystem (Filesystem)
import Trilby.HNix
import Trilby.Util
import Prelude

data Size = GiB Int | Whole
    deriving stock (Generic, Eq, Show)

instance ToExpr Size where
    toExpr (GiB gib) = toExpr $ show gib <> "G"
    toExpr Whole = toExpr @String "100%"

data Subvolume = Subvolume
    { name :: Text
    , mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Subvolume where
    toExpr Subvolume{..} =
        [nix|
        {
            mountpoint = mountpoint;
            mountOptions = mountoptions;
        }
        |]

data PartitionContent
    = BtrfsPartition {subvolumes :: [Subvolume]}
    | EfiPartition {filesystem :: Filesystem}
    | FilesystemPartition {filesystem :: Filesystem}
    | LuksPartition {name :: Text, keyFile :: Maybe Text, content :: PartitionContent}
    deriving stock (Generic, Show, Eq)

instance ToExpr PartitionContent where
    toExpr BtrfsPartition{..} =
        [nix|
        {
            type = "btrfs";
            subvolumes = subvolumesSet;
        }
        |]
      where
        subvolumesSet = listToSet (fromText . doubleQuoted . (.name)) subvolumes
    toExpr EfiPartition{..} = toExpr filesystem
    toExpr FilesystemPartition{..} = toExpr filesystem
    toExpr LuksPartition{..} =
        [nix|
        {
            type = "luks";
            name = name;
            content = content;
            settings.keyFile = keyFile;
        }
        |]

data Partition = Partition
    { name :: Text
    , size :: Size
    , content :: PartitionContent
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Partition where
    toExpr Partition{..} =
        canonicalSet
            [nix|
            {
                size = size;
                type = typ;
                content = content;
            }
            |]
      where
        typ = toExpr $ case content of
            EfiPartition{} -> Just @String "EF00"
            _ -> Nothing
