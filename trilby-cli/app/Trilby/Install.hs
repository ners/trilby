{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Install where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Shelly
import System.Posix (getFileStatus, isBlockDevice)
import Trilby.Config (Edition (..))
import Trilby.Options
import Trilby.Util
import Prelude hiding (error)

getDisk :: Sh Text
getDisk = do
    disk <- liftIO do
        putStrLn "Choose installation disk:"
        Text.getLine
    diskStatus <- liftIO $ getFileStatus $ Text.unpack disk
    if isBlockDevice diskStatus
        then pure disk
        else do
            liftIO $ error $ "cannot find disk: " <> disk
            getDisk

getOpts :: InstallOpts Maybe -> InstallOpts Sh
getOpts opts = do
    InstallOpts
        { efi = maybe (ask "Use EFI boot?" True) pure opts.efi
        , luks = maybe (ask "Encrypt the disk with LUKS2?" True) pure opts.luks
        , disk = maybe getDisk pure opts.disk
        , format = maybe (ask "Format the disk?" True) pure opts.format
        , edition = maybe (pure Workstation) pure opts.edition
        , host = maybe (liftIO $ putStrLn "Choose hostname:" >> Text.getLine) pure opts.host
        , username = maybe (liftIO $ putStrLn "Choose admin username:" >> Text.getLine) pure opts.username
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

install :: InstallOpts Maybe -> IO ()
install (getOpts -> opts) = shelly do
    whenM opts.format $ doFormat opts
    rootIsMounted <- do
        errExit False $ cmd "mountpoint" "-q" rootMount
        (0 ==) <$> lastExitCode
    unless rootIsMounted $ errorExit "/mnt is not a mountpoint"
    let dir = rootMount <> "/etc/trilby"
    cmd "mkdir" "-p" dir
    cd $ Text.unpack dir
    host <- opts.host
    username <- opts.username
    cmd "mkdir" "-p" ("hosts/" <> host) ("users/" <> username)

doFormat :: InstallOpts Sh -> Sh ()
doFormat opts = do
    disk <- opts.disk
    cmd "sgdisk" "--zap-all" "-o" disk
    efi <- opts.efi
    when efi do
        cmd "sgdisk" "-n" "0:0:+1G" "-t" "0:EF00" "-c" ("0:" <> efiLabel) disk
    let rootPartNum = if efi then 1 else 0
    luks <- opts.luks
    let rootLabel = if luks then luksLabel else trilbyLabel
    cmd "sgdisk" "-n" (tshow rootPartNum <> ":0:+1G") "-t" (tshow rootPartNum <> ":8300") "-c" (tshow rootPartNum <> ":" <> rootLabel) disk
    cmd "partprobe"
    when efi do
        cmd "mkfs.fat" "-F32" "-n" efiLabel efiDevice
    when luks do
        cmd "cryptsetup" "luksFormat" luksDevice
        cmd "cryptsetup" "luksOpen" luksDevice luksName
    cmd "mkfs.btrfs" "-f" "-L" trilbyLabel $ if luks then luksOpenDevice else trilbyDevice
    cmd "partprobe"
    cmd "mount" trilbyDevice rootMount
    cmd "btrfs" "subvolume" "create" rootVol
    cmd "btrfs" "subvolume" "create" homeVol
    cmd "btrfs" "subvolume" "create" nixVol
    when (not efi) do
        cmd "btrfs" "subvolume" "create" bootVol
    cmd "unmount" rootMount
    cmd "mount" "-o" "subvol=root,compress=zstd,noatime" trilbyDevice rootMount
    cmd "mkdir" "-p" bootVol homeVol nixVol
    if efi
        then cmd "mount" efiDevice bootVol
        else cmd "mount" "-o" "subvol=boot" trilbyDevice bootVol
    cmd "mount" "-o" "subvol=home,compress=zstd" trilbyDevice homeVol
    cmd "mount" "-o" "subvol=nix,compress=zstd,noatime" trilbyDevice nixVol
