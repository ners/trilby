{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Control.Applicative (empty)
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (Default (def))
import Data.Functor ((<&>))
import Data.List qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Channel (Channel (Unstable))
import Trilby.Config.Host
import Trilby.Config.User
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

applySubstitutions :: [(Text, Text)] -> Text -> Text
applySubstitutions = flip $ Data.List.foldr $ uncurry Text.replace

install :: (MonadIO m) => InstallOpts Maybe -> m ()
install (askOpts -> opts) = do
    whenM opts.format $ doFormat opts
    rootIsMounted <- sudo ("mountpoint -q " <> rootMount) <&> (== ExitSuccess)
    unless rootIsMounted do
        unlessM (ask "Attempt to mount the partitions?" True) $ errorExit "/mnt is not a mountpoint"
        doMount opts
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
    writeNixFile flake "flake.nix"
    let user = User{uid = 1000, ..}
    writeNixFile user $ fromText userDir <> "/default.nix"
    let host =
            Host
                { channel = Unstable
                , keyboardLayout = "us"
                , timezone = "Europe/Zurich"
                , ..
                }
    writeNixFile host $ fromText hostDir <> "/default.nix"
    output (fromText hostDir <> "/hardware-configuration.nix") $
        inshell ("sudo nixos-generate-config --show-hardware-config --root " <> rootMount) stdin
    sudo_ $
        Text.unwords
            [ "nixos-install"
            , "--flake " <> trilbyDir <> "#" <> hostname
            , "--no-root-password"
            , "--impure"
            ]
    whenM opts.reboot $ sudo_ "reboot"

doFormat :: (MonadIO m) => InstallOpts m -> m ()
doFormat opts = do
    disk <- opts.disk
    sudo_ $ "sgdisk --zap-all -o " <> disk
    efi <- opts.efi
    when efi do
        sudo_ $
            Text.unwords
                [ "sgdisk"
                , "-n 1:0:+1G"
                , "-t 1:EF00"
                , "-c 1:" <> efiLabel <> " " <> disk
                ]
    luks <- opts.luks
    let useLuks = case luks of
            UseLuks{} -> True
            _ -> False
    let rootPartNum = if efi then 2 else 1 :: Int
    let rootLabel = if useLuks then luksLabel else trilbyLabel
    sudo_ $
        Text.unwords
            [ "sgdisk"
            , "-n " <> tshow rootPartNum <> ":0:0"
            , "-t " <> tshow rootPartNum <> ":8300"
            , "-c " <> tshow rootPartNum <> ":" <> rootLabel
            , disk
            ]
    sudo_ "partprobe"
    when efi do
        sudo_ $ "mkfs.fat -F32 -n" <> efiLabel <> " " <> efiDevice
    when useLuks do
        luks.luksPassword >>= liftIO . Text.writeFile (fromText luksPasswordFile)
        sudo_ $ "cryptsetup luksFormat --type luks2 -d " <> luksPasswordFile <> " " <> luksDevice
        sudo_ $ "cryptsetup luksOpen -d " <> luksPasswordFile <> " " <> luksDevice <> " " <> luksName
        rm $ fromText luksPasswordFile
    let rootDevice = if useLuks then luksOpenDevice else trilbyDevice
    sudo_ $ "mkfs.btrfs -f -L " <> trilbyLabel <> " " <> rootDevice
    sudo_ "partprobe"
    sudo_ $ "mount " <> rootDevice <> " " <> rootMount
    sudo_ $ "btrfs subvolume create " <> rootVol
    sudo_ $ "btrfs subvolume create " <> homeVol
    sudo_ $ "btrfs subvolume create " <> nixVol
    unless efi do
        sudo_ $ "btrfs subvolume create " <> bootVol
    sudo_ $ "umount " <> rootMount
    sudo_ $ "mount -o subvol=root,ssd,compress=zstd,noatime " <> rootDevice <> " " <> rootMount
    sudo_ $ "mkdir -p " <> Text.unwords [bootVol, homeVol, nixVol]
    if efi
        then sudo_ $ "mount " <> efiDevice <> " " <> bootVol
        else sudo_ $ "mount -o subvol=boot " <> rootDevice <> " " <> bootVol
    sudo_ $ "mount -o subvol=home,ssd,compress=zstd " <> rootDevice <> " " <> homeVol
    sudo_ $ "mount -o subvol=nix,ssd,compress=zstd,noatime " <> rootDevice <> " " <> nixVol

doMount :: (MonadIO m) => InstallOpts m -> m ()
doMount opts = do
    efi <- opts.efi
    luks <- opts.luks
    let useLuks = case luks of
            UseLuks{} -> True
            _ -> False
    when useLuks do
        luks.luksPassword >>= liftIO . Text.writeFile (fromText luksPasswordFile)
        sudo_ $ "cryptsetup luksOpen -d " <> luksPasswordFile <> " " <> luksDevice <> " " <> luksName
        rm $ fromText luksPasswordFile
    let rootDevice = if useLuks then luksOpenDevice else trilbyDevice
    sudo_ $ "mount -o subvol=root,compress=zstd,noatime " <> rootDevice <> " " <> rootMount
    sudo_ $ "mkdir -p " <> Text.unwords [bootVol, homeVol, nixVol]
    if efi
        then sudo_ $ "mount " <> efiDevice <> " " <> bootVol
        else sudo_ $ "mount -o subvol=boot " <> rootDevice <> " " <> bootVol
    sudo_ $ "mount -o subvol=home,compress=zstd " <> rootDevice <> " " <> homeVol
    sudo_ $ "mount -o subvol=nix,compress=zstd,noatime " <> rootDevice <> " " <> nixVol
