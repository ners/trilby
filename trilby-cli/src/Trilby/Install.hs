module Trilby.Install (install) where

import Data.Text qualified as Text
import System.FilePath.Lens (directory)
import Trilby.Disko (Disko)
import Trilby.HNix (FlakeRef (..), currentSystem, writeNixFile)
import Trilby.Install.Config.Host
import Trilby.Install.Config.User
import Trilby.Install.Disko
import Trilby.Install.Disko qualified as Disko
import Trilby.Install.Flake
import Trilby.Install.Options
import Trilby.Widgets
import Turtle ((</>))
import Prelude

rootMount :: FilePath
rootMount = "/mnt"

trilbyDir :: FilePath
trilbyDir = rootMount </> "etc/trilby"

diskoFile :: FilePath
diskoFile = "/tmp/disko.nix"

install :: InstallOpts Maybe -> App ()
install (askOpts -> opts) | Just FlakeOpts{..} <- opts.flake = do
    let diskoRef = Disko.Flake flakeRef
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    nixosInstall flakeRef
    whenM copyFlake do
        storePath <- Text.strip <$> shell ("nix flake archive --json " <> flakeRef.url <> " | jq --raw-output .path") empty
        asRoot cmd_ ["cp", "-r", storePath, fromString trilbyDir]
        asRoot cmd_ ["chown", "-R", "1000:1000", fromString trilbyDir]
    reboot opts.reboot
install (askOpts -> opts) = do
    disko <- getDisko opts
    inDir (diskoFile ^. directory) $ writeNixFile diskoFile disko
    let diskoRef = Disko.File diskoFile
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    flakeRef <- setupHost disko opts
    nixosInstall flakeRef
    reboot opts.reboot

formatDisk :: App Bool -> FileOrFlake -> App ()
formatDisk f d = whenM f do
    logWarn "Formatting disk ... "
    disko $ Format d

mountRoot :: FileOrFlake -> App ()
mountRoot d = unlessM rootIsMounted do
    logWarn "Partitions are not mounted"
    unlessM (yesNoButtons "Attempt to mount the partitions?" True) $
        errorExit "Cannot install without mounted partitions"
    disko $ Mount d
  where
    rootIsMounted = (ExitSuccess ==) . fst <$> cmd' ["mountpoint", "-q", fromString rootMount]

setupHost :: Disko -> InstallOpts App -> App FlakeRef
setupHost disko opts = do
    hostname <- opts.hostname
    edition <- opts.edition
    release <- opts.release
    inDir trilbyDir do
        asRoot cmd_ ["chown", "-R", "1000:1000", fromString trilbyDir]
        writeNixFile "flake.nix" $ flake release
        username <- opts.username
        password <- do
            rawPassword <- opts.password
            HashedPassword . firstLine <$> proc ["mkpasswd", rawPassword] empty
        let user = User{uid = 1000, ..}
        let userDir = "users" </> fromText username
        inDir userDir $ writeNixFile "default.nix" user
        keyboard <- opts.keyboard
        locale <- opts.locale
        timezone <- opts.timezone
        let host = Host{..}
        let hostDir = "hosts" </> fromText hostname
        inDir hostDir do
            platform <- currentSystem
            writeNixFile
                "default.nix"
                [nix|
                { lib, ... }:
                lib.trilbySystem {
                  trilby = {
                    edition = edition;
                    buildPlatform = platform;
                    hostPlatform = platform;
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
            writeNixFile "disko.nix" $ sanitise disko
        cmd_ . sconcat $
            [ ["nix", "flake", "lock"]
            , ["--accept-flake-config"]
            , ["--override-input", "trilby", "trilby"]
            ]
    pure FlakeRef{url = fromString trilbyDir, output = pure hostname}

nixosInstall :: FlakeRef -> App ()
nixosInstall flakeRef = do
    logWarn "Performing installation ... "
    -- TODO(vkleen): this shouldn't work and neither should it be necessary ...
    (withTrace . asRoot) rawCmd_ . sconcat $
        [ ["nix", "build"]
        , ["--store", "/mnt"]
        , ["--impure"]
        , ["--accept-flake-config"]
        , ["trilby#nix-monitored"]
        ]
    (withTrace . asRoot) rawCmd_ . sconcat $
        [ ["nixos-install"]
        , ["--flake", ishow flakeRef]
        , ["--option", "accept-flake-config", "true"]
        , ["--no-root-password"]
        , ["--impure"]
        ]

reboot :: App Bool -> App ()
reboot r = whenM r $ cmd_ ["systemctl", "reboot"]
