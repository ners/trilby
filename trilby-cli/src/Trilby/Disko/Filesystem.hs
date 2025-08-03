module Trilby.Disko.Filesystem where

import Trilby.Prelude

data Format
    = Btrfs
    | Ext4
    | Fat32
    | XFS
    deriving stock (Generic, Show, Eq, Bounded, Enum)

instance Read Format where readPrec = readPrecBoundedEnumOn (fmap toLower)

instance ToExpr Format where
    toExpr Fat32 = toExpr @String "vfat"
    toExpr fs = toExpr $ toLower <$> show fs

data Filesystem = Filesystem
    { format :: Format
    , mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic)

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
