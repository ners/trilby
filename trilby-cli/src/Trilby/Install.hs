{-# LANGUAGE UndecidableInstances #-}

module Trilby.Install (install) where

import Control.Monad.Reader qualified as Reader
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text.Encoding qualified as Text
import System.Process.Typed qualified as Process
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

newtype WithPath b t = WithPath {path :: Path b t}
    deriving stock (Generic)

deriving anyclass instance (FromJSON (Path b t)) => FromJSON (WithPath b t)

installLinux :: InstallOpts App -> App ()
installLinux opts | Just FlakeOpts{..} <- opts.flake = do
    let diskoRef = Disko.Flake flakeRef
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    nixosInstall flakeRef
    whenM copyFlake do
        WithPath (storePath :: Path Abs Dir) <- readProcessOutJson' ["nix", "flake", "archive", "--json", flakeRef.url]
        asRoot runProcess'_ ["cp", "-r", fromPath storePath, fromPath $ trilbyDir Linux]
        asRoot runProcess'_ ["chown", "-R", "1000:1000", fromPath $ trilbyDir Linux]
    reboot opts.reboot Localhost
installLinux opts = do
    disko <- getDisko opts
    let diskoFile = $(mkRelFile "disko.nix")
    tmpDiskoFile <- Reader.asks $ tmpDir >>> (</> diskoFile)
    writeNixFile tmpDiskoFile disko
    let tmpDiskoRef = Disko.File . Abs $ tmpDiskoFile
    formatDisk opts.format tmpDiskoRef
    mountRoot tmpDiskoRef
    flakeRef <- setupHost Linux opts \hostDir _ ->
        writeNixFile (hostDir </> diskoFile) $ sanitise disko
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
    rootIsMounted = (ExitSuccess ==) . fst <$> readProcess' ["mountpoint", "-q", fromPath $ rootMount Linux]

flakeNix, defaultNix, configurationNix :: Path Rel File
flakeNix = $(mkRelFile "flake.nix")
defaultNix = $(mkRelFile "default.nix")
configurationNix = $(mkRelFile "configuration.nix")

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
                    uid <- read . fromText <$> readProcessOutText' ["id", "-u"]
                    gid <- read . fromText <$> readProcessOutText' ["id", "-g"]
                    pure Owner{..}
        asRoot runProcess'_ ["chown", "-R", ishow owner, fromPath realTrilbyDir]
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
            case kernel of
                Linux -> do
                    writeFile $(mkRelFile "hardware-configuration.nix")
                        =<< asRoot
                            readProcessOutText'
                            [ "nixos-generate-config"
                            , "--show-hardware-config"
                            , "--no-filesystems"
                            , "--root"
                            , fromPath $ rootMount kernel
                            ]
                    runProcess'_ . sconcat $
                        [ ["nix", "flake", "lock"]
                        , ["--accept-flake-config"]
                        , ["--override-input", "trilby", "trilby"]
                        ]
                Darwin -> pure ()
        actions hostDir userDir
        whenM opts.edit do
            editor <- fromMaybe "nano" <$> lookupEnv "EDITOR"
            runProcess'_
                [ fromString editor
                , "flake.nix"
                , fromPath $ hostDir </> configurationNix
                ]
    pure FlakeRef{url = fromPath realTrilbyDir, output = pure hostname}

hashedPassword :: Text -> App Password
hashedPassword plain =
    HashedPassword
        . firstLine
        . Text.decodeUtf8
        . LazyByteString.toStrict
        <$> Process.readProcessStdout_ (Process.proc "mkpasswd" [fromText plain])

nixosInstall :: FlakeRef -> App ()
nixosInstall flakeRef = do
    logWarn "Performing installation ... "
    -- TODO(vkleen): this shouldn't work and neither should it be necessary ...
    (withTrace . asRoot) runProcess'_ . sconcat $
        [ ["nix", "build"]
        , ["--store", "/mnt"]
        , ["--impure"]
        , ["--accept-flake-config"]
        , ["trilby#nix-monitored"]
        ]
    (withTrace . asRoot) runProcess'_ . sconcat $
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
    runProcess'_ ["nix", "run", ishow darwinRebuild, "--", "switch", "--flake", ishow flakeRef]
