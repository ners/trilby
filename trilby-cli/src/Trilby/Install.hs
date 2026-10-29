module Trilby.Install (install, hashedPassword) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Effectful.Reader.Static qualified as Reader
import Trilby.Configuration
    ( ConfigAction (..)
    , buildConfiguration
    , buildConfigurationFrom
    , switchToConfiguration
    )
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (writeNixFile)
import Trilby.Host (Host (Localhost), onHost)
import Trilby.Host qualified as Host
import Trilby.Install.Config.Host
import Trilby.Install.Config.Release (Release (..))
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
install rawOpts =
    hostSystem >>= \case
        System{kernel = Linux} -> do
            ensureDeps
                [ ("disko", "disko")
                , ("mkpasswd", "mkpasswd")
                ]
            installLinux opts
        System{kernel = Darwin} -> do
            when (isJust rawOpts.flake)
                $ for_ @[]
                    [ ("--disk", isJust rawOpts.disk)
                    , ("--format", isJust rawOpts.format)
                    , ("--luks", isJust rawOpts.luks)
                    , ("--filesystem", isJust rawOpts.filesystem)
                    ]
                    \(option, given) -> when given $ logAttention_ ("Ignoring " <> option <> ": it does not apply on Darwin")
            installDarwin opts
  where
    opts = askOpts rawOpts

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
    copy <- copyFlake
    reboot <- opts.reboot
    format <- opts.format
    let diskoRef = Disko.Flake flakeRef
    formatDisk (pure format) diskoRef
    mountRoot diskoRef
    nixosInstall flakeRef
    when copy $ copyFlakeTo Linux flakeRef
    when reboot Configuration.reboot
installLinux opts0 = withTempFile $(mkRelFile "disko.nix") \diskoFile -> do
    disko <- getDisko opts0
    opts <- resolveHostOpts Linux opts0
    format <- opts.format
    inDir (parent diskoFile) $ writeNixFile diskoFile disko
    let diskoRef = Disko.File $ Abs diskoFile
    formatDisk (pure format) diskoRef
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
                    let user = User{uid = max 1000 owner.uid, ..}
                    keyboard <- Just <$> opts.keyboard
                    locale <- Just <$> opts.locale
                    timezone <- Just <$> opts.timezone
                    stateVersion <- localStateVersion release
                    bios <- (BIOS ==) <$> opts.bootMode
                    pure (user, Host{..})
                Darwin -> do
                    let user = User{uid = max 1000 owner.uid, username, password = Nothing}
                    let host =
                            Host
                                { keyboard = Nothing
                                , locale = Nothing
                                , timezone = Nothing
                                , stateVersion = Nothing
                                , bios = False
                                , ..
                                }
                    pure (user, host)

        userDir <- parseRelDir . fromText $ "users/" <> username
        let userFile = userDir </> defaultNix
        writeNixFile userFile user
        inDir hostDir do
            platform <- show <$> onHost Localhost hostSystem
            writeNixFile defaultNix $ systemModule edition platform
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
                    overrides <- ifM isOffline (offlineOverrides release) (pure [])
                    command <-
                        nixCommand ["flake", "lock", "--accept-flake-config", "--override-input", "trilby", "trilby"]
                    cmd_
                        $ NonEmpty.appendList
                            command
                            (overrides >>= \(name, path) -> ["--override-input", name, "path:" <> path])
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
       , TypedProcess :> es
       , Log :> es
       )
    => Text
    -> Eff es Password
hashedPassword plain = do
    let p =
            setStdin (byteStringInput . LazyByteString.fromStrict $ Encoding.encodeUtf8 plain)
                $ proc ["mkpasswd", "--method=yescrypt", "--stdin"]
    out <- Text.strip . Encoding.decodeUtf8 . LazyByteString.toStrict <$> readProcessStdout_ p
    case Text.lines out of
        (h : _) | "$y$" `Text.isPrefixOf` h -> pure $ HashedPassword h
        _ -> fail "mkpasswd failed to produce a yescrypt hash"

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
    (withTrace . withOfflineEnv . asRoot) cmd_
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
installDarwin opts | Just FlakeOpts{..} <- opts.flake = do
    configuration <- Configuration.fromHost =<< Reader.ask
    name <- case flakeRef.output of
        [] -> pure configuration.name
        [name] -> pure name
        output ->
            errorExit
                $ "Cannot install from a flake output that is not a single configuration name: "
                <> ishow flakeRef{output}
    result <- buildConfigurationFrom ["--impure"] flakeRef.url configuration{Configuration.name}
    switchToConfiguration result ConfigSwitch
    whenM copyFlake $ copyFlakeTo Darwin flakeRef
    whenM opts.reboot Configuration.reboot
installDarwin opts0 = do
    opts <- resolveHostOpts Darwin opts0
    configuration <- Configuration.fromHost =<< Reader.ask
    FlakeRef{output = [name]} <- setupHost Darwin opts \_ _ -> pure ()
    flip switchToConfiguration ConfigSwitch =<< buildConfiguration configuration{Configuration.name}
    whenM opts.reboot Configuration.reboot

copyFlakeTo
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Kernel
    -> FlakeRef
    -> Eff es ()
copyFlakeTo kernel flakeRef = do
    archive <- cmdOutJson =<< nixCommand ["flake", "archive", "--json", flakeRef.url]
    storePath <-
        maybe (errorExit "nix flake archive did not report a store path") pure
            $ Aeson.parseMaybe (Aeson.withObject "archive" (Aeson..: "path")) (archive :: Aeson.Value)
    owner <- case kernel of
        Linux -> pure "1000:1000"
        Darwin -> ishow <$> currentOwner
    let dest = fromPath $ trilbyDir kernel
    asRoot cmd_ ["mkdir", "-p", dest]
    asRoot cmd_ ["cp", "-R", storePath <> "/.", dest]
    asRoot cmd_ ["chmod", "-R", "u+w", dest]
    asRoot cmd_ ["chown", "-R", owner, dest]

memoOpt
    :: (Monad m)
    => Lens' (InstallOpts m) (m a)
    -> InstallOpts m
    -> m (InstallOpts m)
memoOpt l opts = do
    a <- opts ^. l
    pure $ opts & l .~ pure a

resolveHostOpts :: (Monad m) => Kernel -> InstallOpts m -> m (InstallOpts m)
resolveHostOpts kernel =
    memoOpt #hostname
        >=> memoOpt #edition
        >=> memoOpt #release
        >=> memoOpt #username
        >=> when' (kernel == Linux) (memoOpt #password)
        >=> when' (kernel == Linux) (memoOpt #keyboard)
        >=> when' (kernel == Linux) (memoOpt #locale)
        >=> when' (kernel == Linux) (memoOpt #timezone)
        >=> when' (kernel == Linux) (memoOpt #bootMode)
        >=> memoOpt #edit
        >=> memoOpt #reboot
  where
    when' :: (Applicative f) => Bool -> (a -> f a) -> a -> f a
    when' c f = if c then f else pure

localStateVersion
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Release
    -> Eff es (Maybe Text)
localStateVersion Unstable = do
    (code, out) <-
        cmdCodeOutText =<< nixCommand ["eval", "--offline", "--raw", "nixpkgs#lib.trivial.release"]
    pure $ case (code, Text.strip out) of
        (ExitSuccess, v) | not (Text.null v) -> Just v
        _ -> Nothing
localStateVersion release = pure . Just $ ishow release

newtype ArchivedInput = ArchivedInput {path :: Text}
    deriving stock (Generic)
    deriving anyclass (Aeson.FromJSON)

newtype ArchivedInputs = ArchivedInputs {inputs :: Map Text ArchivedInput}
    deriving stock (Generic)
    deriving anyclass (Aeson.FromJSON)

offlineOverrides
    :: ( HasCallStack
       , IOE :> es
       , Reader AppState :> es
       , Reader Host.Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Release
    -> Eff es [(Text, Text)]
offlineOverrides release = do
    ArchivedInputs{inputs} <-
        cmdOutJson =<< nixCommand ["flake", "archive", "--json", "--dry-run", "trilby"]
    let archived name = (name,) . (.path) <$> Map.lookup name inputs
    pure . catMaybes $ case release of
        Unstable -> archived <$> ["nixpkgs", "home-manager", "nixvim"]
        _ -> [("nixpkgs-unstable",) . (.path) <$> Map.lookup "nixpkgs" inputs]
