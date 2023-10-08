{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Install where

import Control.Applicative (empty)
import Control.Monad
import Control.Monad.Extra (ifM, unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Data.List qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Language.Haskell.TH qualified as TH
import System.Posix (getFileStatus)
import Trilby.Config.Edition
import Trilby.Options
import Trilby.Util
import Turtle (ExitCode (ExitSuccess))
import Turtle.Prelude hiding (shell)
import Prelude hiding (error)

getDisk :: (MonadIO m) => m Text
getDisk = do
    disks <- fmap ("/dev/" <>) . Text.lines <$> strict (inshell "lsblk --raw | grep '\\W\\+disk\\W\\+$' | awk '{print $1}'" empty)
    disk <- choose "Choose installation disk:" disks
    diskStatus <- liftIO $ getFileStatus $ Text.unpack disk
    if isBlockDevice diskStatus
        then pure disk
        else do
            error $ "Cannot find disk: " <> disk
            getDisk

getLuks :: (MonadIO m) => Maybe (LuksOpts Maybe) -> m (LuksOpts m)
getLuks opts = ifM askLuks (pure UseLuks{..}) (pure NoLuks)
  where
    askLuks = maybe (ask "Encrypt the disk with LUKS2?" True) (const $ pure True) opts
    luksPassword = maybe (prompt "Choose LUKS password:") pure (opts >>= (.luksPassword))

getEdition :: (MonadIO m) => m Edition
getEdition = choose "Choose edition:" [Workstation, Server]

getOpts :: forall m. (MonadIO m) => InstallOpts Maybe -> InstallOpts m
getOpts opts = do
    InstallOpts
        { efi = maybe (ask "Use EFI boot?" True) pure opts.efi
        , luks = getLuks opts.luks
        , disk = maybe getDisk pure opts.disk
        , format = maybe (ask "Format the disk?" True) pure opts.format
        , edition = maybe getEdition pure opts.edition
        , hostname = maybe (prompt "Choose hostname:") pure opts.hostname
        , username = maybe (prompt "Choose admin username:") pure opts.username
        , password = maybe (prompt "Choose admin password:") pure opts.password
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

flakeTemplate :: Text
flakeTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/flake.nix")

hostTemplate :: Text
hostTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/host.nix")

userTemplate :: Text
userTemplate = $(TH.stringE . Text.unpack <=< TH.runIO . Text.readFile $ "assets/install/user.nix")

applySubstitutions :: [(Text, Text)] -> Text -> Text
applySubstitutions = flip $ Data.List.foldr $ uncurry Text.replace

install :: (MonadIO m) => InstallOpts Maybe -> m ()
install (getOpts -> opts) = do
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
    password <- opts.password
    initialHashedPassword <- fmap Text.strip . strict $ inshell ("mkpasswd '" <> password <> "'") empty
    edition <- opts.edition
    let hostDir = "hosts/" <> hostname
    let userDir = "users/" <> username
    shell_ ("mkdir -p " <> hostDir <> " " <> userDir) empty
    let
        substitute :: Text -> Text
        substitute =
            applySubstitutions
                [ ("$hostname", hostname)
                , ("$username", username)
                , ("$initialHashedPassword", initialHashedPassword)
                , ("$edition", tshow edition)
                , ("$channel", "unstable")
                ]
    output "flake.nix" $ toLines $ pure $ substitute flakeTemplate
    output (fromText $ hostDir <> "/default.nix") $ toLines $ pure $ substitute hostTemplate
    output (fromText $ userDir <> "/default.nix") $ toLines $ pure $ substitute userTemplate
    output (fromText $ hostDir <> "/hardware-configuration.nix") $
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
    let rootPartNum = if efi then 2 else 1
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
