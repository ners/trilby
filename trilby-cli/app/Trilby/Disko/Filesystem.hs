module Trilby.Disko.Filesystem where

import Data.Fix (Fix (Fix))
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix
import Nix.TH (ToExpr (toExpr))
import Trilby.HNix
import Trilby.Util
import Prelude

data Format
    = Btrfs
    | Ext4
    | Fat32
    | XFS
    deriving stock (Generic, Show, Eq)

instance ToExpr Format where
    toExpr :: Format -> NExprLoc
    toExpr f = NStrAnn fakeSrcSpan $ fromString $ show f

data Filesystem = Filesystem
    { format :: Format
    , mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Filesystem where
    toExpr :: Filesystem -> NExprLoc
    toExpr f =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "format" ~: toExpr f.format
            , "mountpoint" ~: t f.mountpoint
            , "mountoptions" ~:: NListAnnF fakeSrcSpan (t <$> f.mountoptions)
            ]
      where
        t = Fix . NStrAnnF fakeSrcSpan . fromText
