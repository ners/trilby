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

data PartitionType = EfiPartition | LinuxPartition
    deriving stock (Generic, Eq, Show)

data PartitionSize = Sectors Int | GiB Int | Percent Int
    deriving stock (Generic, Eq, Show)

data PartitionConfig = PartitionConfig
    { partitionType :: PartitionType
    , partitionSize :: PartitionSize
    , bootable :: Bool
    , encrypted :: Bool
    , filesystem :: Filesystem
    , label :: Text
    , content :: Either Mount [SubvolumeConfig]
    }
    deriving stock (Generic, Eq, Show)

data DiskFormat = Gpt
    deriving stock (Generic, Eq, Show)

data DiskConfig = DiskConfig
    { path :: Text
    , format :: DiskFormat
    , partitions :: [PartitionConfig]
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
                , format = Gpt
                , partitions =
                    [ PartitionConfig
                        { partitionType = EfiPartition
                        , partitionSize = GiB 1
                        , encrypted = False
                        , bootable = True
                        , filesystem = Fat32
                        , label = "EFI"
                        , content = Left Mount{mountPoint = "/boot", mountFlags = []}
                        }
                    , PartitionConfig
                        { partitionType = LinuxPartition
                        , partitionSize = Percent 100
                        , encrypted = True
                        , bootable = False
                        , filesystem = Btrfs
                        , label = "Trilby"
                        , content =
                            Right
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
            ]
        }
