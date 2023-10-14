module Trilby.Install.Disko where

import Control.Lens
import Control.Monad (when)
import Data.Generics.Labels ()
import Data.String (IsString)
import Trilby.App (App)
import Trilby.Disko
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.Install.Options
import Trilby.Util
import Prelude hiding (writeFile)

diskoFile :: (IsString s) => s
diskoFile = "/tmp/disko.nix"

luksPasswordFile :: (IsString s) => s
luksPasswordFile = "/tmp/luksPassword"

data DiskoAction = Format | Mount

runDisko :: DiskoAction -> App ()
runDisko Format = quietSudo_ ["disko", "-m", "disko", diskoFile]
runDisko Mount = quietSudo_ ["disko", "-m", "mount", diskoFile]

getDisko :: InstallOpts App -> App Disko
getDisko opts = do
    disk <- opts.disk
    luks <- opts.luks
    let useLuks = luks `is` #_UseLuks
    when useLuks $ writeFile luksPasswordFile =<< luks.luksPassword
    filesystem <- opts.filesystem
    let mbrPartition =
            Partition
                { label = "boot"
                , size = Trilby.Disko.Partition.GiB 1
                , content = MbrPartition
                }
    let efiPartition =
            Partition
                { label = "EFI"
                , size = Trilby.Disko.Partition.GiB 1
                , content =
                    EfiPartition
                        Filesystem
                            { format = Fat32
                            , mountpoint = "/boot"
                            , mountoptions = []
                            }
                }
    let rootContent =
            case filesystem of
                Btrfs ->
                    BtrfsPartition
                        [ Subvolume
                            { name = "root"
                            , mountpoint = "/"
                            , mountoptions = ["compress=zstd", "noatime"]
                            }
                        , Subvolume
                            { name = "home"
                            , mountpoint = "/home"
                            , mountoptions = ["compress=zstd"]
                            }
                        , Subvolume
                            { name = "nix"
                            , mountpoint = "/nix"
                            , mountoptions = ["compress=zstd", "noatime"]
                            }
                        ]
                format ->
                    FilesystemPartition
                        { filesystem =
                            Filesystem
                                { format
                                , mountpoint = "/"
                                , mountoptions = []
                                }
                        }
    let rootLuksContent =
            LuksPartition
                { name = "cryptroot"
                , keyFile = Just luksPasswordFile
                , content = rootContent
                }
    let rootPartition =
            Partition
                { label = "Trilby"
                , size = Whole
                , content = if useLuks then rootLuksContent else rootContent
                }
    pure
        Disko
            { disks =
                [ Disk
                    { device = disk
                    , content =
                        Gpt
                            [ mbrPartition
                            , efiPartition
                            , rootPartition
                            ]
                    }
                ]
            }

clearLuksFiles :: Disko -> Disko
clearLuksFiles = #disks . traverse . #content . #partitions . traverse . #content %~ clear
  where
    clear LuksPartition{..} = LuksPartition{keyFile = Nothing, ..}
    clear x = x
