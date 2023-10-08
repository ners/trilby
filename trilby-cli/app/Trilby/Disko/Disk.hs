module Trilby.Disko.Disk where

import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.Disko.Partition (Partition)
import Prelude

newtype DiskContent = Gpt {partitions :: [Partition]}
    deriving stock (Generic, Show, Eq)

instance ToExpr DiskContent where
    toExpr Gpt{..} =
        [nix|
        {
            type = "gpt";
            partitions = partitions;
        }
        |]

data Disk = Disk
    { device :: Text
    , content :: DiskContent
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Disk where
    toExpr Disk{..} =
        [nix|
        {
            type = "disk";
            device = device;
            content = content;
        }
        |]
