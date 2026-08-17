module Trilby.Install (install) where

import Effectful.Reader.Static qualified as Reader
import Trilby.Configuration (ConfigAction (..), buildConfiguration, switchToConfiguration)
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (writeNixFile)
import Trilby.Host (Host (Localhost), onHost)
import Trilby.Host qualified as Host
import Trilby.Install.Config.Host
import Trilby.Install.Config.User
import Trilby.Install.Disko
import Trilby.Install.Disko qualified as Disko
import Trilby.Install.Flake
import Trilby.Install.Options
import Trilby.Prelude
import Trilby.Setup (ensureDeps)
import Trilby.Widgets

rootMount :: Kernel -> Path Abs Dir
rootMount Linux = $(mkAbsDir "/mnt")
rootMount Darwin = $(mkAbsDir "/")

trilbyDir :: Kernel -> Path Abs Dir
trilbyDir = trilbyHome . rootMount

install
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => InstallOpts Maybe
    -> Eff es ()
install (askOpts -> opts) =
    hostSystem >>= \case
        System{kernel = Linux} -> installLinux opts
        System{kernel = Darwin} -> installDarwin opts

installLinux
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => InstallOpts (Eff es)
    -> Eff es ()
installLinux opts | Just FlakeOpts{..} <- opts.flake = do
    ensureDeps
        [ ("disko", "disko")
        , ("mkpasswd", "mkpasswd")
        ]
    let diskoRef = Disko.Flake flakeRef
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    nixosInstall flakeRef
    whenM copyFlake do
        Just storePath <-
            shellOutTextFirstLine ["nix flake archive --json " <> flakeRef.url <> " | jq --raw-output .path"]
        asRoot cmd_ ["cp", "-r", storePath, fromPath $ trilbyDir Linux]
        asRoot cmd_ ["chown", "-R", "1000:1000", fromPath $ trilbyDir Linux]
    whenM opts.reboot Configuration.reboot
installLinux opts = withTempFile $(mkRelFile "disko.nix") \diskoFile -> do
    disko <- getDisko opts
    inDir (parent diskoFile) $ writeNixFile diskoFile disko
    let diskoRef = Disko.File $ Abs diskoFile
    formatDisk opts.format diskoRef
    mountRoot diskoRef
    flakeRef <- setupHost Linux opts $ \hostDir _ -> do
        inDir hostDir $ writeNixFile $(mkRelFile "disko.nix") $ sanitise disko
    nixosInstall flakeRef
    whenM opts.reboot Configuration.reboot

formatDisk
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Eff es Bool
    -> FileOrFlake
    -> Eff es ()
formatDisk f d = whenM f do
    logAttention_ "Formatting disk ... "
    disko $ Format d

mountRoot
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , Concurrent :> es
       , TypedProcess :> es
       , Log :> es
       )
    => FileOrFlake
    -> Eff es ()
mountRoot d = unlessM rootIsMounted do
    logAttention_ "Partitions are not mounted"
    unlessM (yesNoButtons "Attempt to mount the partitions?" True)
        $ errorExit "Cannot install without mounted partitions"
    disko $ Mount d
  where
    rootIsMounted
        :: ( HasCallStack
           , IOE :> es
           , Reader AppState :> es
           , Reader Host.Host :> es
           , TypedProcess :> es
           , Log :> es
           )
        => Eff es Bool
    rootIsMounted = (ExitSuccess ==) <$> cmdCode ["mountpoint", "-q", fromPath $ rootMount Linux]

flakeNix, defaultNix, configurationNix :: Path Rel File
flakeNix = $(mkRelFile "flake.nix")
defaultNix = $(mkRelFile "default.nix")
configurationNix = $(mkRelFile "configuration.nix")

setupHost
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => Kernel
    -> InstallOpts (Eff es)
    -> (Path Rel Dir -> Path Rel Dir -> Eff es ())
    -> Eff es FlakeRef
setupHost kernel opts actions = do
    hostname <- opts.hostname
    edition <- opts.edition
    release <- opts.release
    realTrilbyDir <- canonicalizePath $ trilbyDir kernel
    inDir realTrilbyDir do
        owner <- currentOwner
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
            platform <- show <$> onHost Localhost hostSystem
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
                            cmdOutText
                            [ "nixos-generate-config"
                            , "--show-hardware-config"
                            , "--no-filesystems"
                            , "--root"
                            , fromPath $ rootMount kernel
                            ]
                    cmd_
                        . sconcat
                        $ [ ["nix", "flake", "lock"]
                          , ["--accept-flake-config"]
                          , ["--override-input", "trilby", "trilby"]
                          ]
                Darwin -> pure ()
        actions hostDir userDir
        whenM opts.edit do
            editor <- fromMaybe "nano" <$> lookupEnv "EDITOR"
            (runProcess_ . proc)
                [ fromString editor
                , "flake.nix"
                , fromPath $ hostDir </> configurationNix
                ]
    pure FlakeRef{url = fromPath realTrilbyDir, output = pure hostname}

hashedPassword
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Text
    -> Eff es Password
hashedPassword plain =
    maybe (fail "mkpasswd failed") (pure . HashedPassword) =<< cmdOutTextFirstLine ["mkpasswd", plain]

nixosInstall
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => FlakeRef
    -> Eff es ()
nixosInstall flakeRef = do
    logAttention_ "Performing installation ... "
    -- TODO(vkleen): this shouldn't work and neither should it be necessary ...
    (withTrace . asRoot) cmd_
        . sconcat
        $ [ ["nix", "build"]
          , ["--store", "/mnt"]
          , ["--impure"]
          , ["--accept-flake-config"]
          , ["trilby#nix-monitored"]
          ]
    (withTrace . asRoot) cmd_
        . sconcat
        $ [ ["nixos-install"]
          , ["--flake", ishow flakeRef]
          , ["--option", "accept-flake-config", "true"]
          , ["--no-root-password"]
          , ["--impure"]
          ]

installDarwin
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => InstallOpts (Eff es)
    -> Eff es ()
-- installDarwin opts | Just FlakeOpts{..} <- opts.flake = pure ()
installDarwin opts = do
    configuration <- Configuration.fromHost =<< Reader.ask
    FlakeRef{output = [name]} <- setupHost Darwin opts \_ _ -> pure ()
    flip switchToConfiguration ConfigSwitch =<< buildConfiguration configuration{Configuration.name}
