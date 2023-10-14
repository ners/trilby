{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Control.Monad
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.Logger (logWarn)
import Data.Default (Default (def))
import Data.Generics.Labels ()
import Data.Generics.Sum.Typed ()
import Data.Text qualified as Text
import Trilby.App (App)
import Trilby.Config.Host
import Trilby.Config.User
import Trilby.Install.Disko
import Trilby.Install.Flake
import Trilby.Install.Options
import Trilby.Util
import Turtle (Alternative (empty), ExitCode (ExitSuccess), IsString (fromString), cd, directory, realpath)
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

install :: InstallOpts Maybe -> App ()
install (askOpts -> opts) = do
    disko <- getDisko opts
    inDir (directory diskoFile) $ writeNixFile diskoFile disko
    whenM opts.format $ do
        $(logWarn) "Formatting disk"
        runDisko Format
    let rootIsMounted = (ExitSuccess ==) . fst <$> sudo' ["mountpoint", "-q", rootMount]
    unlessM rootIsMounted do
        $(logWarn) "Partitions are not mounted"
        unlessM (askYesNo "Attempt to mount the partitions?" True) $
            errorExit "Cannot install without mounted partitions"
        runDisko Mount
    $(logWarn) "Performing installation"
    inDir trilbyDir $
        sudo_ ["chown", "-R", "1000:1000", trilbyDir]
    cd $ fromText trilbyDir
    username <- opts.username
    password <- do
        rawPassword <- opts.password
        HashedPassword . Text.strip <$> proc ["mkpasswd", rawPassword] empty
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
        writeNixFile "disko.nix" $ clearLuksFiles disko
    cmd_
        . flip append ["nix", "flake", "lock", "--override-input", "trilby"]
        . fromString
        =<< realpath "/etc/trilby"
    sudo_
        [ "nixos-install"
        , "--flake"
        , ".#" <> hostname
        , "--no-root-password"
        , "--impure"
        ]
    whenM opts.reboot $ sudo_ ["reboot"]
