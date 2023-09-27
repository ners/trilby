module Trilby.Disko.Disk where

import Data.Text (Text)
import GHC.Generics (Generic)
import Nix
import Nix.TH (ToExpr (toExpr))
import Trilby.Disko.Partition (Partition)
import Trilby.HNix
import Trilby.Util
import Prelude

newtype DiskContent = Gpt {partitions :: [Partition]}
    deriving stock (Generic, Show, Eq)

instance ToExpr DiskContent where
    toExpr Gpt{..} =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "type" ~:: NStrAnnF fakeSrcSpan "gpt"
            , "partitions" ~:: NListAnnF fakeSrcSpan (toExpr <$> partitions)
            ]

data Disk = Disk
    { device :: Text
    , content :: DiskContent
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Disk where
    toExpr Disk{..} =
        NSetAnn
            fakeSrcSpan
            NonRecursive
            [ "type" ~:: NStrAnnF fakeSrcSpan "disk"
            , "device" ~:: NStrAnnF fakeSrcSpan (fromText device)
            , "content" ~: toExpr content
            ]
