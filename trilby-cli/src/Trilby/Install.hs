{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Install where

import Data.Text qualified as Text
import Internal.Prelude
import System.FilePath.Lens (directory)
import Trilby.Config.Host
import Trilby.Config.User
import Trilby.Install.Disko
import Trilby.Install.Disko qualified as Disko
import Trilby.Install.Flake
import Trilby.Install.Options

showNix :: (ToExpr e, IsString s) => e -> s
showNix = fromString . show . prettyNix . toExpr

writeNixFile :: (ToExpr a) => FilePath -> a -> App ()
writeNixFile f = writeFile f . showNix

efiLabel :: (IsString s) => s
efiLabel = "EFI"

luksLabel :: (IsString s) => s
luksLabel = "LUKS"

trilbyLabel :: (IsString s) => s
trilbyLabel = "Trilby"

luksDevice :: FilePath
luksDevice = "/dev/disk/by-partlabel/" <> luksLabel

luksName :: (IsString s) => s
luksName = "cryptroot"

luksOpenDevice :: FilePath
luksOpenDevice = "/dev/mapper/" <> luksName

trilbyDevice :: FilePath
trilbyDevice = "/dev/disk/by-partlabel/" <> trilbyLabel

efiDevice :: FilePath
efiDevice = "/dev/disk/by-partlabel/" <> efiLabel

rootMount :: FilePath
rootMount = "/mnt"

rootVol :: FilePath
rootVol = rootMount <> "/root"

bootVol :: FilePath
bootVol = rootMount <> "/boot"

homeVol :: FilePath
homeVol = rootMount <> "/home"

nixVol :: FilePath
nixVol = rootMount <> "/nix"

trilbyDir :: FilePath
trilbyDir = rootMount <> "/etc/trilby"

install :: InstallOpts Maybe -> App ()
install (askOpts -> opts) | Just FlakeOpts{..} <- opts.flake = do
    whenM opts.format do
        $(logWarn) "Formatting disk"
        runDisko $ Format $ Disko.Flake flakeRef
    let rootIsMounted = (ExitSuccess ==) . fst <$> cmd' ["mountpoint", "-q", fromString rootMount]
    unlessM rootIsMounted do
        $(logWarn) "Partitions are not mounted"
        unlessM (askYesNo "Attempt to mount the partitions?" True) $
            errorExit "Cannot install without mounted partitions"
        runDisko $ Mount $ Disko.Flake flakeRef
    (withTrace . asRoot)
        cmd_
        [ "nixos-install"
        , "--flake"
        , flakeRef
        , "--no-root-password"
        , "--impure"
        ]
    whenM copyFlake do
        let flakeUri = Text.takeWhile (/= '#') flakeRef
        storePath <- Text.strip <$> shell ("nix flake archive --json " <> flakeUri <> " | jq --raw-output .path") empty
        asRoot cmd_ ["cp", "-r", storePath, fromString trilbyDir]
        asRoot cmd_ ["chown", "-R", "1000:1000", fromString trilbyDir]
    whenM opts.reboot $ asRoot cmd_ ["reboot"]
install (askOpts -> opts) = do
    disko <- getDisko opts
    inDir (diskoFile ^. directory) $ writeNixFile diskoFile disko
    whenM opts.format $ do
        $(logWarn) "Formatting disk ... "
        runDisko $ Format $ Disko.File diskoFile
    let rootIsMounted = (ExitSuccess ==) . fst <$> cmd' ["mountpoint", "-q", fromString rootMount]
    unlessM rootIsMounted do
        $(logWarn) "Partitions are not mounted"
        unlessM (askYesNo "Attempt to mount the partitions?" True) $
            errorExit "Cannot install without mounted partitions"
        runDisko $ Mount $ Disko.File diskoFile
    inDir trilbyDir do
        asRoot cmd_ ["chown", "-R", "1000:1000", fromString trilbyDir]
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
        keyboard <- opts.keyboard
        locale <- opts.locale
        timezone <- opts.timezone
        let host = Host{..}
        let hostDir = "hosts/" <> fromText hostname
        inDir hostDir do
            writeNixFile
                "default.nix"
                [nix|
                { lib, ... }:

                lib.trilbySystem {
                  trilby = {
                    edition = edition;
                    channel = channel;
                  };
                  modules = lib.findModulesList ./.;
                }
                |]
            writeNixFile "configuration.nix" host
            writeFile "hardware-configuration.nix"
                =<< asRoot
                    cmd
                    [ "nixos-generate-config"
                    , "--show-hardware-config"
                    , "--no-filesystems"
                    , "--root"
                    , fromString rootMount
                    ]
            writeNixFile "disko.nix" $ clearLuksFiles disko
        $(logWarn) "Performing installation ... "
        cmd_ ["nix", "flake", "lock", "--override-input", "trilby", "trilby"]
        (withTrace . asRoot)
            rawCmd_
            [ "nixos-install"
            , "--flake"
            , ".#" <> hostname
            , "--no-root-password"
            , "--impure"
            ]
        whenM opts.reboot $ asRoot cmd_ ["reboot"]
