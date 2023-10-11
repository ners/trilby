{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Control.Applicative (empty)
import Control.Lens
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Trilby.Config.Channel (Channel (Unstable))
import Trilby.Config.Host
import Trilby.Config.User
import Trilby.Disko
import Trilby.Disko.Disk
import Trilby.Disko.Filesystem
import Trilby.Disko.Partition
import Trilby.Install.Flake (Flake)
import Trilby.Install.Options
import Trilby.Util
import Turtle.Prelude hiding (shell)
import Prelude hiding (error)
import Control.Monad.IO.Class (MonadIO)
import Turtle (ExitCode(ExitSuccess))

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
    hostname <- opts.hostname
    username <- opts.username
    rawPassword <- opts.password
    password <- HashedPassword . Text.strip <$> inshellstrict ("mkpasswd " <> singleQuoted rawPassword) empty
    edition <- opts.edition
    let hostDir = "hosts/" <> hostname
    let userDir = "users/" <> username
    flip shell_ empty $
        Text.unwords
            [ "mkdir"
            , "-p"
            , hostDir
            , userDir
            ]
    let
    let flake = def @Flake
    writeNixFile "flake.nix" flake
    let user = User{uid = 1000, ..}
    writeNixFile (fromText userDir <> "/default.nix") user
    let host =
            Host
                { channel = Unstable
                , keyboardLayout = "us"
                , timezone = "Europe/Zurich"
                , ..
                }
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
    efi <- opts.efi
    luks <- opts.luks
    useLuks <-
        case luks of
            UseLuks{..} -> do
                output (fromText luksPasswordFile) . toLines . pure =<< luksPassword
                pure True
            NoLuks -> pure False
    filesystem <- opts.filesystem
    let bootFs =
            Filesystem
                { format = Fat32
                , mountpoint = "/mnt/boot"
                , mountoptions = []
                }
    let bootPartition =
            if efi
                then
                    Partition
                        { name = "ESP"
                        , size = Trilby.Disko.Partition.GiB 1
                        , content =
                            EfiPartition
                                { filesystem = bootFs
                                }
                        }
                else
                    Partition
                        { name = "boot"
                        , size = Trilby.Disko.Partition.GiB 1
                        , content =
                            FilesystemPartition
                                { filesystem = bootFs
                                }
                        }
    let rootContent =
            case filesystem of
                Btrfs ->
                    BtrfsPartition
                        { subvolumes =
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
                        }
                format -> FilesystemPartition{filesystem = Filesystem{format, mountpoint = "/", mountoptions = []}}
    let rootPartition =
            Partition
                { name = "Trilby"
                , size = Whole
                , content =
                    if useLuks
                        then
                            LuksPartition
                                { name = "LUKS"
                                , keyFile = Just luksPasswordFile
                                , content = rootContent
                                }
                        else rootContent
                }
    pure
        Disko
            { disks =
                [ Disk
                    { device = disk
                    , content = Gpt{partitions = [bootPartition, rootPartition]}
                    }
                ]
            }
