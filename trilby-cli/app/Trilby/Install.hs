{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Control.Lens
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Generics.Sum.Typed ()
import Data.Text qualified as Text
import Trilby.App (App)
import Trilby.Config.Host
import Trilby.Config.User
import Trilby.Disko
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.Install.Flake (Flake)
import Trilby.Install.Options
import Trilby.Util
import Turtle (ExitCode (ExitSuccess), IsString (fromString), directory)
import Turtle.Prelude hiding (shell)
import Prelude hiding (error, writeFile)

efiLabel :: (IsString s) => s
efiLabel = "EFI"

luksLabel :: (IsString s) => s
luksLabel = "LUKS"

trilbyLabel :: (IsString s) => s
trilbyLabel = "Trilby"

luksDevice :: (IsString s) => s
luksDevice = fromString $ "/dev/disk/by-partlabel/" <> luksLabel

luksName :: (IsString s) => s
luksName = "cryptroot"

luksOpenDevice :: (IsString s) => s
luksOpenDevice = fromString $ "/dev/mapper/" <> luksName

trilbyDevice :: (IsString s) => s
trilbyDevice = fromString $ "/dev/disk/by-partlabel/" <> trilbyLabel

efiDevice :: (IsString s) => s
efiDevice = fromString $ "/dev/disk/by-partlabel/" <> efiLabel

rootMount :: (IsString s) => s
rootMount = "/mnt"

rootVol :: (IsString s) => s
rootVol = fromString $ rootMount <> "/root"

bootVol :: (IsString s) => s
bootVol = fromString $ rootMount <> "/boot"

homeVol :: (IsString s) => s
homeVol = fromString $ rootMount <> "/home"

nixVol :: (IsString s) => s
nixVol = fromString $ rootMount <> "/nix"

trilbyDir :: (IsString s) => s
trilbyDir = fromString $ rootMount <> "/etc/trilby"

luksPasswordFile :: (IsString s) => s
luksPasswordFile = "/tmp/luksPassword"

diskoFile :: (IsString s) => s
diskoFile = "/tmp/disko.nix"

install :: InstallOpts Maybe -> App ()
install (askOpts -> opts) = do
    disko <- getDisko opts
    inDir (directory diskoFile) $ writeNixFile diskoFile disko
    whenM opts.format $ sudo_ ["disko", "-m", "disko", diskoFile]
    let rootIsMounted = (ExitSuccess ==) . fst <$> sudo' ["mountpoint", "-q", rootMount]
    unlessM rootIsMounted do
        unlessM (askYesNo "Attempt to mount the partitions?" True) $
            errorExit "/mnt is not a mountpoint"
        sudo_ ["disko", "-m", "mount", diskoFile]
    inDir trilbyDir $
        sudo_ ["chown", "-R", "1000:1000", trilbyDir]
    cd $ fromText trilbyDir
    username <- opts.username
    password <- do
        rawPassword <- opts.password
        hash <- Text.strip <$> cmd ["mkpasswd", rawPassword]
        pure $ HashedPassword hash
    writeNixFile "flake.nix" $ def @Flake
    let user = User{uid = 1000, ..}
    let userDir = "users/" <> fromText username
    inDir userDir $ writeNixFile "default.nix" user
    hostname <- opts.hostname
    edition <- opts.edition
    channel <- opts.channel
    let host =
            Host
                { keyboardLayout = "us"
                , timezone = "Europe/Zurich"
                , ..
                }
    let hostDir = "hosts/" <> fromText hostname
    inDir hostDir do
        writeNixFile "default.nix" host
        writeFile "hardware-configuration.nix"
            =<< sudo
                [ "nixos-generate-config"
                , "--show-hardware-config"
                , "--no-filesystems"
                , "--root"
                , rootMount
                ]
        writeNixFile "disko.nix" $
            disko
                & #disks
                    . traverse
                    . #content
                    . #partitions
                    . traverse
                    . #content
                    . #_LuksPartition
                    . _2
                    .~ Nothing
    sudo_
        [ "nixos-install"
        , "--flake"
        , trilbyDir <> "#" <> hostname
        , "--no-root-password"
        , "--impure"
        ]
    whenM opts.reboot $ sudo_ ["reboot"]

getDisko :: InstallOpts App -> App Disko
getDisko opts = do
    disk <- opts.disk
    luks <- opts.luks
    let useLuks = luks `is` #_UseLuks
    when useLuks $ writeFile luksPasswordFile =<< luks.luksPassword
    filesystem <- opts.filesystem
    let mbrPartition =
            Partition
                { name = "boot"
                , size = Trilby.Disko.Partition.GiB 1
                , content = MbrPartition
                }
    let efiPartition =
            Partition
                { name = "ESP"
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
                { name = "LUKS"
                , keyFile = Just luksPasswordFile
                , content = rootContent
                }
    let rootPartition =
            Partition
                { name = "Trilby"
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
