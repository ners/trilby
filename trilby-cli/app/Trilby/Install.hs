{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Install where

import Control.Applicative (empty)
import Control.Monad
import Control.Monad.Extra (fromMaybeM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Data.List qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Language.Haskell.TH qualified as TH
import System.Posix (getFileStatus)
import Text.Read (readMaybe)
import Trilby.Config (Edition (..))
import Trilby.Options
import Trilby.Util
import Turtle (ExitCode (ExitSuccess))
import Turtle.Prelude hiding (shell, shells)
import Prelude hiding (error)

getDisk :: (MonadIO m) => m Text
getDisk = do
    disk <- prompt "Choose installation disk:"
    diskStatus <- liftIO $ getFileStatus $ Text.unpack disk
    if isBlockDevice diskStatus
        then pure disk
        else do
            error $ "Cannot find disk: " <> disk
            getDisk

getEdition :: (MonadIO m) => m Edition
getEdition =
    fromMaybeM (error "Unknown edition" >> getEdition) $
        readMaybe . Text.unpack <$> prompt "Choose edition:"

getOpts :: (MonadIO m) => InstallOpts Maybe -> InstallOpts m
getOpts opts = do
    InstallOpts
        { efi = maybe (ask "Use EFI boot?" True) pure opts.efi
        , luks = maybe (ask "Encrypt the disk with LUKS2?" True) pure opts.luks
        , luksPassword = maybe (prompt "LUKS password:") pure opts.luksPassword
        , disk = maybe getDisk pure opts.disk
        , format = maybe (ask "Format the disk?" True) pure opts.format
        , edition = maybe getEdition pure opts.edition
        , host = maybe (prompt "Choose hostname:") pure opts.host
        , username = maybe (prompt "Choose admin username:") pure opts.username
        , reboot = maybe (ask "Reboot system?" True) pure opts.reboot
        }

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
trilbyDevice = "/dev/disk/by-label/" <> trilbyLabel

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

flakeTemplate :: Text
flakeTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/flake.nix")

hostTemplate :: Text
hostTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/host.nix")

userTemplate :: Text
userTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/user.nix")

applySubstitutions :: [(Text, Text)] -> Text -> Text
applySubstitutions subs str =
    Data.List.foldl' (\acc (from, to) -> Text.replace from to acc) str subs

install :: InstallOpts Maybe -> IO ()
install (getOpts -> opts) = do
    whenM opts.format $ doFormat opts
    rootIsMounted <- shell ("sudo mountpoint -q " <> rootMount) stdin <&> (== ExitSuccess)
    unless rootIsMounted $ errorExit "/mnt is not a mountpoint"
    sudo $ "mkdir -p " <> trilbyDir
    cd $ Text.unpack trilbyDir
    host <- opts.host
    username <- opts.username
    edition <- opts.edition
    let hostDir = "hosts/" <> host
    let userDir = "users/" <> username
    sudo $ "mkdir -p " <> hostDir <> " " <> userDir
    let
        substitute :: Text -> Text
        substitute =
            applySubstitutions
                [ ("$hostname", host)
                , ("$username", username)
                , ("$edition", tshow edition)
                , ("$channel", "unstable")
                ]
    output "flake.nix" $ toLines $ pure $ substitute flakeTemplate
    output (Text.unpack $ hostDir <> "/default.nix") $ toLines $ pure $ substitute hostTemplate
    output (Text.unpack $ userDir <> "/default.nix") $ toLines $ pure $ substitute userTemplate
    output (Text.unpack $ hostDir <> "/hardware.nix") $
        inshell ("nixos-generate-config --show-hardware-config --root " <> rootMount) empty
    shells ("nixos-install --flake " <> trilbyDir <> "#" <> host <> " --no-root-password") empty
    whenM opts.reboot $ sudo "reboot"

doFormat :: (MonadIO m) => InstallOpts m -> m ()
doFormat opts = do
    disk <- opts.disk
    sudo $ "sgdisk --zap-all -o " <> disk
    efi <- opts.efi
    when efi do
        sudo $
            Text.unwords
                [ "sgdisk"
                , "-n 1:0:+1G"
                , "-t 1:EF00"
                , "-c 1:" <> efiLabel <> " " <> disk
                ]
    luks <- opts.luks
    let rootPartNum = if efi then 2 else 1
    let rootLabel = if luks then luksLabel else trilbyLabel
    sudo $
        Text.unwords
            [ "sgdisk"
            , "-n " <> tshow rootPartNum <> ":0:0"
            , "-t " <> tshow rootPartNum <> ":8300"
            , "-c " <> tshow rootPartNum <> ":" <> rootLabel
            , disk
            ]
    sudo "partprobe"
    when efi do
        sudo $ "mkfs.fat -F32 -n" <> efiLabel <> " " <> efiDevice
    when luks do
        -- _password <- opts.luksPassword
        sudo $ "cryptsetup luksFormat --type luks2 " <> luksDevice
        sudo $ "cryptsetup luksOpen " <> luksDevice <> " " <> luksName
    sudo $ "mkfs.btrfs -f -L " <> trilbyLabel <> " " <> if luks then luksOpenDevice else trilbyDevice
    shells "partprobe" stdin
    sudo $ "mount " <> trilbyDevice <> " " <> rootMount
    sudo $ "btrfs subvolume create " <> rootVol
    sudo $ "btrfs subvolume create " <> homeVol
    sudo $ "btrfs subvolume create " <> nixVol
    unless efi do
        sudo $ "btrfs subvolume create " <> bootVol
    sudo $ "unmount " <> rootMount
    sudo $ "mount -o subvol=root,compress=zstd,noatime " <> trilbyDevice <> " " <> rootMount
    sudo $ "mkdir -p " <> Text.unwords [bootVol, homeVol, nixVol]
    if efi
        then sudo $ "mount " <> efiDevice <> " " <> bootVol
        else sudo $ "mount -o subvol=boot " <> trilbyDevice <> " " <> bootVol
    sudo $ "mount -o subvol=home,compress=zstd " <> trilbyDevice <> " " <> homeVol
    sudo $ "mount -o subvol=nix,compress=zstd,noatime " <> trilbyDevice <> " " <> nixVol
