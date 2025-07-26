module Trilby.Install (install) where

import Data.Text qualified as Text
import Trilby.HNix (writeNixFile)
import Trilby.Host (Host (Localhost), hostSystem, reboot)
import Trilby.Install.Config.Host
import Trilby.Install.Config.User
import Trilby.Install.Disko
import Trilby.Install.Disko qualified as Disko
import Trilby.Install.Flake
import Trilby.Install.Options
import Trilby.System
import Trilby.Widgets
import Turtle qualified
import Prelude

rootMount :: Kernel -> Path Abs Dir
rootMount Linux = $(mkAbsDir "/mnt")
rootMount Darwin = $(mkAbsDir "/")

trilbyDir :: Kernel -> Path Abs Dir
trilbyDir = trilbyHome . rootMount

install :: InstallOpts Maybe -> App ()
install (askOpts -> opts) = do
    system <- hostSystem Localhost
    case system.kernel of
        Linux -> installLinux opts
        Darwin -> installDarwin opts

installLinux :: InstallOpts App -> App ()
installLinux opts | Just FlakeOpts{..} <- opts.flake = do
    let diskoRef = Disko.Flake flakeRef
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    nixosInstall flakeRef
    whenM copyFlake do
        storePath <- Text.strip <$> shell ("nix flake archive --json " <> flakeRef.url <> " | jq --raw-output .path") empty
        asRoot cmd_ ["cp", "-r", storePath, fromPath $ trilbyDir Linux]
        asRoot cmd_ ["chown", "-R", "1000:1000", fromPath $ trilbyDir Linux]
    reboot opts.reboot Localhost
installLinux opts = withTempFile $(mkRelFile "disko.nix") \diskoFile -> do
    disko <- getDisko opts
    inDir (parent diskoFile) $ writeNixFile diskoFile disko
    let diskoRef = Disko.File $ Abs diskoFile
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    flakeRef <- setupHost Linux opts $ \hostDir _ -> do
        inDir hostDir $ writeNixFile $(mkRelFile "disko.nix") $ sanitise disko
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
    rootIsMounted = (ExitSuccess ==) . fst <$> cmd' ["mountpoint", "-q", fromPath $ rootMount Linux]

flakeNix, defaultNix, configurationNix :: Path Rel File
flakeNix = $(mkRelFile "flake.nix")
defaultNix = $(mkRelFile "default.nix")
configurationNix = $(mkRelFile "configuration.nix")

data Owner = Owner {uid :: Int, gid :: Int}

instance Show Owner where
    show Owner{..} = show uid <> ":" <> show gid

setupHost
    :: Kernel
    -> InstallOpts App
    -> (Path Rel Dir -> Path Rel Dir -> App ())
    -> App FlakeRef
setupHost kernel opts actions = do
    hostname <- opts.hostname
    edition <- opts.edition
    release <- opts.release
    realTrilbyDir <- canonicalizePath $ trilbyDir kernel
    inDir realTrilbyDir do
        owner <-
            case kernel of
                Linux -> pure Owner{uid = 1000, gid = 1000}
                Darwin -> do
                    uid <- read . fromText <$> cmd ["id", "-u"]
                    gid <- read . fromText <$> cmd ["id", "-g"]
                    pure Owner{..}
        asRoot cmd_ ["chown", "-R", ishow owner, fromPath realTrilbyDir]
        writeNixFile flakeNix $ flake kernel release
        hostDir <- parseRelDir . fromText $ "hosts/" <> hostname
        username <- opts.username
        (user, host) <-
            case kernel of
                Linux -> do
                    password <- Just <$> (hashedPassword =<< opts.password)
                    let user = User{uid = owner.uid, ..}
                    keyboard <- Just <$> opts.keyboard
                    locale <- Just <$> opts.locale
                    timezone <- Just <$> opts.timezone
                    pure (user, Host{..})
                Darwin -> do
                    let user = User{uid = owner.uid, username, password = Nothing}
                    let host = Host{keyboard = Nothing, locale = Nothing, timezone = Nothing, ..}
                    pure (user, host)

        userDir <- parseRelDir . fromText $ "users/" <> username
        let userFile = userDir </> defaultNix
        writeNixFile userFile user
        inDir hostDir do
            platform <- show <$> hostSystem Localhost
            writeNixFile
                defaultNix
                [nix|
                { inputs, lib, ... }:
                lib.trilbySystem {
                  inherit inputs;
                  trilby = {
                    edition = edition;
                    buildPlatform = platform;
                    hostPlatform = platform;
                  };
                  modules = lib.findModulesList ./.;
                }
                |]
            writeNixFile configurationNix host
            case kernel of
                Linux -> do
                    writeFile $(mkRelFile "hardware-configuration.nix")
                        =<< asRoot
                            cmd
                            [ "nixos-generate-config"
                            , "--show-hardware-config"
                            , "--no-filesystems"
                            , "--root"
                            , fromPath $ rootMount kernel
                            ]
                    cmd_ . sconcat $
                        [ ["nix", "flake", "lock"]
                        , ["--accept-flake-config"]
                        , ["--override-input", "trilby", "trilby"]
                        ]
                Darwin -> pure ()
        actions hostDir userDir
        whenM opts.edit do
            editor <- fromMaybe "nano" <$> lookupEnv "EDITOR"
            rawCmd_
                [ fromString editor
                , "flake.nix"
                , fromPath $ hostDir </> configurationNix
                ]
    pure FlakeRef{url = fromPath realTrilbyDir, output = pure hostname}

hashedPassword :: Text -> App Password
hashedPassword plain = HashedPassword . firstLine <$> Turtle.strict (Turtle.inproc "mkpasswd" [plain] empty)

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

installDarwin :: InstallOpts App -> App ()
-- installDarwin opts | Just FlakeOpts{..} <- opts.flake = pure ()
installDarwin opts = do
    flakeRef <- setupHost Darwin opts \_ _ -> pure ()
    let darwinRebuild = flakeRef{output = ["darwin-rebuild"]}
    cmd_ ["nix", "run", ishow darwinRebuild, "--", "switch", "--flake", ishow flakeRef]
