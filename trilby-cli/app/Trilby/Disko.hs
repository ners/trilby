module Trilby.Disko where

import Data.Default (Default (def))
import GHC.Generics (Generic)
import Nix
import Nix.Prelude (One (one))
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.HNix
import Trilby.Util (fromText)
import Prelude

newtype Disko = Disko {disks :: [Disk]}
    deriving stock (Generic, Show, Eq)

instance ToExpr Disko where
    toExpr Disko{..} =
        [nix|
        {
            disko.devices.disk = diskSet;
        }
        |]
      where
        diskSet = listToSet (one . DynamicKey . Plain . fromText . (.device)) disks

instance Default Disko where
    def =
        Disko
            { disks =
                [ Disk
                    { device = "/dev/vda"
                    , content =
                        Gpt
                            { partitions =
                                [ Partition
                                    { name = "ESP"
                                    , size = GiB 1
                                    , content =
                                        EfiPartition
                                            { filesystem =
                                                Filesystem
                                                    { format = Fat32
                                                    , mountpoint = "/boot"
                                                    , mountoptions = []
                                                    }
                                            }
                                    }
                                , Partition
                                    { name = "LUKS"
                                    , size = Whole
                                    , content =
                                        LuksPartition
                                            { name = "Trilby"
                                            , content =
                                                BtrfsPartition
                                                    { subvolumes =
                                                        [ Subvolume
                                                            { name = "/root"
                                                            , mountpoint = "/"
                                                            , mountoptions = ["compress=zstd", "noatime"]
                                                            }
                                                        , Subvolume
                                                            { name = "/home"
                                                            , mountpoint = "/home"
                                                            , mountoptions = ["compress=zstd"]
                                                            }
                                                        , Subvolume
                                                            { name = "/nix"
                                                            , mountpoint = "/nix"
                                                            , mountoptions = ["compress=zstd", "noatime"]
                                                            }
                                                        ]
                                                    }
                                            }
                                    }
                                ]
                            }
                    }
                ]
            }
