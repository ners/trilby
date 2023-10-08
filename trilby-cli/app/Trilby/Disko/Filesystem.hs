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
    toExpr = toExpr . fmap toLower . show

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
            format = format;
            mountpoint = mountpoint;
            mountoptions = mountoptions;
        }
        |]
