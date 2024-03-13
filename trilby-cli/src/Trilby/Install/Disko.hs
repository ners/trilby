module Trilby.Install.Disko
    ( FileOrFlake (..)
    , DiskoAction (..)
    , disko
    , getDisko
    , sanitise
    )
where

import Control.Lens
import Data.Generics.Labels ()
import Data.List (sortOn)
import System.FilePath.Lens
import Trilby.Disko
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.HNix (FileOrFlake (..))
import Trilby.Install.Options
import Prelude

luksPasswordFile :: FilePath
luksPasswordFile = "/tmp/luksPassword"

data DiskoAction
    = Format !FileOrFlake
    | Mount !FileOrFlake
    deriving stock (Generic)

disko :: DiskoAction -> App ()
disko action =
    (withTrace . asRoot) quietCmd_ $ case action of
        Format (File f) -> ["disko", "-m", "disko", fromString f]
        Format (Flake f) -> ["disko", "-m", "disko", "--flake", ishow f]
        Mount (File f) -> ["disko", "-m", "mount", fromString f]
        Mount (Flake f) -> ["disko", "-m", "mount", "--flake", ishow f]

getDisko :: InstallOpts App -> App Disko
getDisko opts = do
    diskName <- opts.disk
    diskDevice <-
        headDef diskName
            . sortOn length
            . lines
            . fromText
            <$> cmd
                ["find", "-L", "/dev/disk/by-id", "-samefile", fromString diskName]
    $(logWarn) $ "Using disk " <> fromString diskName <> " with id " <> fromString diskDevice
    luks <- opts.luks
    let useLuks = luks `is` #_UseLuks
    when useLuks $ writeFile luksPasswordFile =<< luks.luksPassword
    filesystem <- opts.filesystem
    let mbrPartition =
            Partition
                { priority = Just 0
                , label = "boot"
                , size = Trilby.Disko.Partition.MiB 1
                , content = MbrPartition
                }
    let efiPartition =
            Partition
                { priority = Just 1
                , label = "EFI"
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
                , keyFile = Just $ PasswordFile luksPasswordFile
                , content = rootContent
                }
    let rootPartition =
            Partition
                { priority = Nothing
                , label = "Trilby"
                , size = Whole
                , content = if useLuks then rootLuksContent else rootContent
                }
    pure
        Disko
            { disks =
                [ Disk
                    { name = fromString $ diskDevice ^. filename
                    , device = fromString diskDevice
                    , content =
                        Gpt
                            [ mbrPartition
                            , efiPartition
                            , rootPartition
                            ]
                    }
                ]
            }

sanitise :: Disko -> Disko
sanitise = #disks . traverse . #content . #partitions . traverse . #content %~ clearLuks
  where
    clearLuks LuksPartition{..} = LuksPartition{keyFile = Nothing, ..}
    clearLuks x = x
