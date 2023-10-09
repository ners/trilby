module Trilby.Disko.Filesystem where

import Data.Char (toLower)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix.TH
import Prelude

data Format
    = Btrfs
    | Ext4
    | Fat32
    | XFS
    deriving stock (Generic, Show, Eq)

instance ToExpr Format where
    toExpr Fat32 = "vfat"
    toExpr fs = toExpr $ toLower <$> show fs

data Filesystem = Filesystem
    { format :: Format
    , mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Filesystem where
    toExpr Filesystem{..} =
        [nix|
        {
            type = "filesystem";
            format = format;
            mountpoint = mountpoint;
            mountOptions = mountoptions;
        }
        |]
