module Trilby.Disko where

import Data.Fix (Fix (Fix))
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Generics (Generic)
import Nix
import Nix.TH (ToExpr (toExpr))
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.HNix
import Trilby.Util
import Prelude

newtype Disko = Disko {disks :: [Disk]}
    deriving stock (Generic, Show, Eq)

instance ToExpr Disko where
    toExpr :: Disko -> NExprLoc
    toExpr Disko{..} =
        Fix $
            NSetAnnF
                fakeSrcSpan
                NonRecursive
                [ "disko.devices.disk" ~:: NSetAnnF fakeSrcSpan NonRecursive (d <$> disks)
                ]
      where
        d :: Disk -> Binding NExprLoc
        d disk = DynamicKey (Plain $ fromText disk.device) :| [] ~: toExpr disk

defaultDisko :: Disko
defaultDisko =
    Disko
        { disks =
            [ Disk
                { device = "/dev/nvme0n1"
                , content =
                    Gpt
                        { partitions =
                            [ EfiPartition
                                { size = GiB 1
                                , filesystem =
                                    Filesystem
                                        { format = Fat32
                                        , mountpoint = "/boot"
                                        , mountoptions = []
                                        }
                                }
                            , LuksPartition
                                { size = Whole
                                , partitions =
                                    [ BtrfsPartition
                                        { size = Whole
                                        , subvolumes =
                                            [ Subvolume
                                                { mountpoint = "/"
                                                , mountoptions = ["compress=zstd", "noatime"]
                                                }
                                            , Subvolume
                                                { mountpoint = "/home"
                                                , mountoptions = ["compress=zstd"]
                                                }
                                            , Subvolume
                                                { mountpoint = "/nix"
                                                , mountoptions = ["compress=zstd", "noatime"]
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                }
            ]
        }
