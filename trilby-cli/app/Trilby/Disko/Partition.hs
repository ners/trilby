{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Disko.Partition where

import Data.Fix (Fix (Fix))
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix
import Nix.TH (ToExpr (toExpr))
import Trilby.Disko.Filesystem
import Trilby.HNix
import Trilby.Util
import Prelude

data Size = GiB Int | Whole
    deriving stock (Generic, Eq, Show)

instance ToExpr Size where
    toExpr :: Size -> NExprLoc
    toExpr (GiB gib) = Fix $ NStrAnnF fakeSrcSpan $ fromString $ show gib <> "G"
    toExpr Whole = Fix $ NStrAnnF fakeSrcSpan "100%"

data Subvolume = Subvolume
    { mountpoint :: Text
    , mountoptions :: [Text]
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Subvolume where
    toExpr :: Subvolume -> NExprLoc
    toExpr s =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "mountpoint" ~: t s.mountpoint
            , "mountoptions" ~:: NListAnnF fakeSrcSpan (t <$> s.mountoptions)
            ]
      where
        t = Fix . NStrAnnF fakeSrcSpan . fromText

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
    toExpr :: Partition -> NExprLoc
    toExpr EfiPartition{..} =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "size" ~: toExpr size
            , "type" ~:: NStrAnnF fakeSrcSpan "EF00"
            , "content" ~: toExpr filesystem
            ]
    toExpr LuksPartition{..} =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "size" ~: toExpr size
            , "type" ~:: NStrAnnF fakeSrcSpan "luks"
            , "partitions" ~:: NListAnnF fakeSrcSpan (toExpr <$> partitions)
            ]
    toExpr BtrfsPartition{..} =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "size" ~: toExpr size
            , "type" ~:: NStrAnnF fakeSrcSpan "btrfs"
            , "subvolumes" ~:: NListAnnF fakeSrcSpan (toExpr <$> subvolumes)
            ]
    toExpr DataPartition{..} =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "size" ~: toExpr size
            , "type" ~:: NStrAnnF fakeSrcSpan "8300"
            , "content" ~: toExpr filesystem
            ]
