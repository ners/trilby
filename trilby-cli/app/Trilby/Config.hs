{-# LANGUAGE DuplicateRecordFields #-}

module Trilby.Config where

import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

type Username = Text

type Password = Text

data Filesystem = Fat32 | Btrfs | XFS
    deriving stock (Generic, Show, Eq)

data Mount = Mount
    { mountPoint :: Text
    , mountFlags :: [Text]
    }
    deriving stock (Generic, Eq, Show)

data SubvolumeConfig = SubvolumeConfig
    { name :: Text
    , mount :: Mount
    }
    deriving stock (Generic, Eq, Show)

data PartitionType = EfiPartition | SwapPartition | LinuxFilesystem | LuksPartition | LvmPartition
    deriving stock (Generic, Eq, Show)

data PartitionSize = GiB Int | Whole
    deriving stock (Generic, Eq, Show)

data PartitionContent
    = MountPoint Mount
    | Subvolumes [SubvolumeConfig]
    deriving stock (Generic, Eq, Show)

data PartitionConfig = PartitionConfig
    { partitionType :: PartitionType
    , partitionSize :: PartitionSize
    , bootable :: Bool
    , encrypted :: Bool
    , filesystem :: Filesystem
    , label :: Text
    , content :: PartitionContent
    }
    deriving stock (Generic, Eq, Show)

data DiskFormat = Gpt
    deriving stock (Generic, Eq, Show)

data DiskContent = DiskContent
    { format :: DiskFormat
    , partitions :: [PartitionConfig]
    }
    deriving stock (Generic, Show)

data DiskConfig = DiskConfig
    { path :: Text
    , content :: DiskContent
    }
    deriving stock (Generic, Show)

data Edition = Workstation | Server
    deriving stock (Generic, Show, Read, Eq)

data TrilbyConfig = TrilbyConfig
    { keyboardLayout :: Text
    , timezone :: Text
    , edition :: Edition
    , user :: (Username, Password)
    }
    deriving stock (Generic, Show)

data InstallConfig = InstallConfig
    { trilby :: TrilbyConfig
    , disks :: [DiskConfig]
    }
    deriving stock (Generic, Show)

defaultTrilbyConfig :: InstallConfig
defaultTrilbyConfig =
    InstallConfig
        { trilby =
            TrilbyConfig
                { keyboardLayout = "us"
                , timezone = "Europe/Zurich"
                , edition = Workstation
                , user = ("trilby", "trilby")
                }
        , disks =
            [ DiskConfig
                { path = "/dev/sda"
                , content =
                    DiskContent
                        { format = Gpt
                        , partitions =
                            [ PartitionConfig
                                { partitionType = EfiPartition
                                , partitionSize = GiB 1
                                , encrypted = False
                                , bootable = True
                                , filesystem = Fat32
                                , label = "EFI"
                                , content = MountPoint Mount{mountPoint = "/boot", mountFlags = []}
                                }
                            , PartitionConfig
                                { partitionType = LinuxFilesystem
                                , partitionSize = Whole
                                , encrypted = True
                                , bootable = False
                                , filesystem = Btrfs
                                , label = "Trilby"
                                , content =
                                    Subvolumes
                                        [ SubvolumeConfig
                                            { name = "root"
                                            , mount = Mount{mountPoint = "/", mountFlags = ["compress=zstd", "noatime"]}
                                            }
                                        , SubvolumeConfig
                                            { name = "home"
                                            , mount = Mount{mountPoint = "/home", mountFlags = ["compress=zstd"]}
                                            }
                                        , SubvolumeConfig
                                            { name = "nix"
                                            , mount = Mount{mountPoint = "/nix", mountFlags = ["compress=zstd", "noatime"]}
                                            }
                                        ]
                                }
                            ]
                        }
                }
            ]
        }
