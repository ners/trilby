{-# OPTIONS_GHC -Wno-orphans #-}
module Trilby.Disko where

import Data.Fix (Fix (Fix))
import Data.List.NonEmpty (NonEmpty((:|)))
import Nix
import Trilby.Config
import Trilby.Util
import Prelude
import Data.String (IsString(fromString))
import Data.List.Extra qualified as List

fakePos :: SourcePos
fakePos = SourcePos { sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1 }

instance IsString (NAttrPath NExpr) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

infixl 4 ~:
(~:) :: NAttrPath r -> r -> Binding r
k ~: v = NamedVar k v fakePos

infixl 4 ~::
(~::) :: NAttrPath (Fix f) -> f (Fix f) -> Binding (Fix f)
k ~:: v = k ~: Fix v

disko :: [DiskConfig] -> NExpr
disko disks =
    Fix $
        NSet
            NonRecursive
            [ "disko.devices" ~:: NSet NonRecursive (diskVar <$> disks)
            ]
    where
        diskVar :: DiskConfig -> Binding NExpr
        diskVar disk = DynamicKey (Plain $ fromText disk.path) :| [] ~: diskoDisk disk

diskoDisk :: DiskConfig -> NExpr
diskoDisk disk =
    Fix $
        NSet
            NonRecursive
            [ "device" ~:: NStr (fromText disk.path)
            , "type" ~:: NStr "disk"
            , "content" ~: diskoDiskContent disk.content
            ]

diskoDiskContent :: DiskContent -> NExpr
diskoDiskContent content = Fix $ NSet NonRecursive
    [ "type" ~:: format
    , "partitions" ~:: partitions
    ]
    where
        format = NStr $
            case content.format of
                Gpt -> "gpt"
        partitions = NSet NonRecursive $ partitionVar <$> content.partitions
        partitionVar partition = fromText partition.label ~: diskoPartition partition

diskoPartition :: PartitionConfig -> NExpr
diskoPartition partition =
    Fix $
        NSet
            NonRecursive
            [ "label" ~:: label
            , "type" ~:: partitionType
            , "size" ~:: partitionSize
            ]
    where
        partitionType = NStr $
            case partition.partitionType of
                EfiPartition -> "EF00"
                SwapPartition -> "8200"
                LinuxFilesystem -> "8300"
                LuksPartition -> "8309"
                LvmPartition -> "8E00"
        partitionSize = NStr $
            case partition.partitionSize of
                GiB gib -> fromString $ show gib <> "G"
                Whole -> "100%"
        label = NStr $ fromText partition.label
