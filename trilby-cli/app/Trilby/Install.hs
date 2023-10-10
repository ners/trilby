{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Control.Applicative (empty)
import Control.Lens ((%~), (&), (.~), _head)
import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Data.Default (Default (def))
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import Data.List qualified
import Data.Text (Text)
import Data.Text qualified as Text
import Trilby.Config.Channel (Channel (Unstable))
import Trilby.Config.Host
import Trilby.Config.User
import Trilby.Install.Flake (Flake)
import Trilby.Install.Options
import Trilby.Util
import Turtle (ExitCode (ExitSuccess), IsString (fromString), MonadIO)
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
    writeNixFile @Disko (fromText hostDir <> "/disko.nix") def
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
    let disko = def @Disko & #disks . _head %~ #device .~ disk
    let diskoFile = "/tmp/disko.nix" :: FilePath
    writeNixFile diskoFile disko
    sudo_ $ "disko -m disko " <> fromString diskoFile
    pure ()

doMount :: (MonadIO m) => InstallOpts m -> m ()
doMount opts = pure ()
