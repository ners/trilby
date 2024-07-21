module Trilby.Install (install) where

import Data.Text qualified as Text
import Trilby.Disko (Disko)
import Trilby.HNix (FlakeRef (..), currentSystem, writeNixFile)
import Trilby.Host (Host (Localhost), reboot)
import Trilby.Install.Config.Host
import Trilby.Install.Config.User
import Trilby.Install.Disko
import Trilby.Install.Disko qualified as Disko
import Trilby.Install.Flake
import Trilby.Install.Options
import Trilby.Widgets
import Turtle qualified
import Prelude

rootMount :: Path Abs Dir
rootMount = $(mkAbsDir "/mnt")

trilbyDir :: Path Abs Dir
trilbyDir = trilbyHome rootMount

install :: InstallOpts Maybe -> App ()
install (askOpts -> opts) | Just FlakeOpts{..} <- opts.flake = do
    let diskoRef = Disko.Flake flakeRef
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    nixosInstall flakeRef
    whenM copyFlake do
        storePath <- Text.strip <$> shell ("nix flake archive --json " <> flakeRef.url <> " | jq --raw-output .path") empty
        asRoot cmd_ ["cp", "-r", storePath, fromPath trilbyDir]
        asRoot cmd_ ["chown", "-R", "1000:1000", fromPath trilbyDir]
    reboot opts.reboot Localhost
install (askOpts -> opts) = withTempFile $(mkRelFile "disko.nix") \diskoFile -> do
    disko <- getDisko opts
    inDir (parent diskoFile) $ writeNixFile diskoFile disko
    let diskoRef = Disko.File $ Abs diskoFile
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    flakeRef <- setupHost disko opts
    nixosInstall flakeRef
    reboot opts.reboot Localhost

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
    rootIsMounted = (ExitSuccess ==) . fst <$> cmd' ["mountpoint", "-q", fromPath rootMount]

flakeNix, defaultNix, configurationNix :: Path Rel File
flakeNix = $(mkRelFile "flake.nix")
defaultNix = $(mkRelFile "default.nix")
configurationNix = $(mkRelFile "configuration.nix")

setupHost :: Disko -> InstallOpts App -> App FlakeRef
setupHost disko opts = do
    hostname <- opts.hostname
    edition <- opts.edition
    release <- opts.release
    inDir trilbyDir do
        asRoot cmd_ ["chown", "-R", "1000:1000", fromPath trilbyDir]
        writeNixFile flakeNix $ flake release
        username <- opts.username
        password <- do
            rawPassword <- opts.password
            HashedPassword . firstLine <$> Turtle.strict (Turtle.inproc "mkpasswd" [rawPassword] empty)
        let user = User{uid = 1000, ..}
        userDir <- parseRelDir . fromText $ "users/" <> username
        let userFile = userDir </> defaultNix
        writeNixFile userFile user
        keyboard <- opts.keyboard
        locale <- opts.locale
        timezone <- opts.timezone
        let host = Host{..}
        hostDir <- parseRelDir . fromText $ "hosts/" <> hostname
        inDir hostDir do
            platform <- currentSystem
            writeNixFile
                defaultNix
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
            writeNixFile configurationNix host
            writeFile $(mkRelFile "hardware-configuration.nix")
                =<< asRoot
                    cmd
                    [ "nixos-generate-config"
                    , "--show-hardware-config"
                    , "--no-filesystems"
                    , "--root"
                    , fromPath rootMount
                    ]
            writeNixFile $(mkRelFile "disko.nix") $ sanitise disko
        cmd_ . sconcat $
            [ ["nix", "flake", "lock"]
            , ["--accept-flake-config"]
            , ["--override-input", "trilby", "trilby"]
            ]
        whenM opts.edit do
            editor <- getEnv "EDITOR"
            rawCmd_
                [ fromString editor
                , "flake.nix"
                , fromPath $ hostDir </> configurationNix
                , fromPath $ userDir </> defaultNix
                ]
    pure FlakeRef{url = fromPath trilbyDir, output = pure hostname}

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
