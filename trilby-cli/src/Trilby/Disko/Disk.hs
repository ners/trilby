module Trilby.Disko.Disk where

import Trilby.Disko.Partition
import Trilby.HNix
import Prelude

newtype DiskContent = Gpt {partitions :: [Partition]}
    deriving stock (Generic)

instance ToExpr DiskContent where
    toExpr Gpt{..} =
        [nix|
        {
            type = "gpt";
            partitions = partitionsSet;
        }
        |]
      where
        partitionsSet = listToSet (fromText . (.label)) partitions

data Disk = Disk
    { name :: Text
    , device :: Text
    , content :: DiskContent
    }
    deriving stock (Generic)

instance ToExpr Disk where
    toExpr Disk{..} =
        [nix|
        {
            type = "disk";
            device = device;
            content = content;
        }
        |]
