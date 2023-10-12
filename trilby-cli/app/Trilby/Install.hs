{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Control.Applicative (empty)
import Control.Lens
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Trilby.Config.Host
import Trilby.Config.User
import Trilby.Disko
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.Install.Flake (Flake)
import Trilby.Install.Options
import Trilby.Util
import Turtle (ExitCode (ExitSuccess))
import Turtle.Prelude hiding (shell)
import Prelude hiding (error)

efiLabel :: Text
efiLabel = "EFI"

luksLabel :: Text
luksLabel = "LUKS"

trilbyLabel :: Text
trilbyLabel = "Trilby"

luksDevice :: Text
luksDevice = "/dev/disk/by-partlabel/" <> luksLabel

luksName :: Text
luksName = "cryptroot"

luksOpenDevice :: Text
luksOpenDevice = "/dev/mapper/" <> luksName

trilbyDevice :: Text
trilbyDevice = "/dev/disk/by-partlabel/" <> trilbyLabel

efiDevice :: Text
efiDevice = "/dev/disk/by-partlabel/" <> efiLabel

rootMount :: Text
rootMount = "/mnt"

rootVol :: Text
rootVol = rootMount <> "/root"

bootVol :: Text
bootVol = rootMount <> "/boot"

homeVol :: Text
homeVol = rootMount <> "/home"

nixVol :: Text
nixVol = rootMount <> "/nix"

trilbyDir :: Text
trilbyDir = rootMount <> "/etc/trilby"

luksPasswordFile :: Text
luksPasswordFile = "/tmp/luksPassword"

diskoFile :: Text
diskoFile = "/tmp/disko.nix"

install :: (MonadIO m) => InstallOpts Maybe -> m ()
install (askOpts -> opts) = do
    disko <- getDisko opts
    writeNixFile (fromText diskoFile) disko
    whenM opts.format $ sudo_ $ "disko -m disko " <> diskoFile
    rootIsMounted <- sudo ("mountpoint -q " <> rootMount) <&> (== ExitSuccess)
    unless rootIsMounted do
        unlessM (ask "Attempt to mount the partitions?" True) $ errorExit "/mnt is not a mountpoint"
        sudo_ $ "disko -m mount " <> diskoFile
    sudo_ $ "mkdir -p " <> trilbyDir
    sudo_ $ "chown -R 1000:1000 " <> trilbyDir
    cd $ fromText trilbyDir
    username <- opts.username
    rawPassword <- opts.password
    password <- HashedPassword . Text.strip <$> inshellstrict ("mkpasswd " <> singleQuoted rawPassword) empty
    let userDir = "users/" <> username
    flip shell_ empty $
        Text.unwords
            [ "mkdir"
            , "-p"
            , userDir
            ]
    let
    let flake = def @Flake
    writeNixFile "flake.nix" flake
    let user = User{uid = 1000, ..}
    writeNixFile (fromText userDir <> "/default.nix") user
    hostname <- opts.hostname
    edition <- opts.edition
    channel <- opts.channel
    let host =
            Host
                { keyboardLayout = "us"
                , timezone = "Europe/Zurich"
                , ..
                }
    let hostDir = "hosts/" <> hostname
    writeNixFile (fromText hostDir <> "/default.nix") host
    output (fromText hostDir <> "/hardware-configuration.nix") $
        inshell ("sudo nixos-generate-config --show-hardware-config --no-filesystems --root " <> rootMount) stdin
    writeNixFile @Disko (fromText hostDir <> "/disko.nix") disko
    sudo_ $
        Text.unwords
            [ "nixos-install"
            , "--flake " <> trilbyDir <> "#" <> hostname
            , "--no-root-password"
            , "--impure"
            ]
    whenM opts.reboot $ sudo_ "reboot"

getDisko :: (MonadIO m) => InstallOpts m -> m Disko
getDisko opts = do
    disk <- opts.disk
    luks <- opts.luks
    let useLuks = luks `is` #_UseLuks
    when useLuks $ output (fromText luksPasswordFile) . toLines . pure =<< luks.luksPassword
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
                format -> FilesystemPartition{filesystem = Filesystem{format, mountpoint = "/", mountoptions = []}}
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
