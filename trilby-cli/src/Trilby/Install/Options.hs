module Trilby.Install.Options where

import Control.Exception qualified as Exception
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isDigit)
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative
import System.Posix (getFileStatus, isBlockDevice)
import Trilby.Configuration qualified as Configuration
import Trilby.Disko.Filesystem
import Trilby.Host
import Trilby.Install.Config.Edition
import Trilby.Install.Config.Keyboard (Keyboard (..), getAllKeyboards, getCurrentKeyboard)
import Trilby.Install.Config.Release
import Trilby.Prelude
import Trilby.Widgets

data BootMode = EFI | BIOS
    deriving stock (Generic, Show, Eq, Bounded, Enum)

instance Read BootMode where readPrec = readPrecBoundedEnumOn (fmap toLower)

data LuksOpts m
    = NoLuks
    | UseLuks {luksPassword :: m Text, luksPasswordFile :: Maybe FilePath}
    deriving stock (Generic)

deriving stock instance Eq (LuksOpts Maybe)

deriving stock instance Show (LuksOpts Maybe)

parseLuks :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (m (LuksOpts m))
parseLuks f = do
    f
        $ flag' NoLuks (long "no-luks")
        <|> do
            flag' () (long "luks" <> help "Encrypt the disk with LUKS2")
            luksPassword <-
                f $ strOption (long "luks-password" <> metavar "PASSWORD" <> help "The disk encryption password")
            luksPasswordFile <-
                optional $ strOption (long "luks-password-file" <> metavar "FILE" <> help "Read the disk encryption password from FILE")
            pure UseLuks{..}

data FlakeOpts m = FlakeOpts {flakeRef :: FlakeRef, copyFlake :: m Bool}
    deriving stock (Generic)

deriving stock instance Show (FlakeOpts Maybe)

parseFlake :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (Maybe (FlakeOpts m))
parseFlake f = do
    optional do
        flakeRef <-
            strOption
                (long "flake" <> metavar "FLAKE" <> help "Build the Trilby system from the specified flake")
        copyFlake <- f $ parseYesNo "copy-flake" "Copy the installation flake to /etc/trilby"
        pure FlakeOpts{..}

data InstallOpts m = InstallOpts
    { flake :: Maybe (FlakeOpts m)
    , luks :: m (LuksOpts m)
    , disk :: m (Path Abs File)
    , bootMode :: m BootMode
    , format :: m Bool
    , filesystem :: m Format
    , edition :: m Edition
    , release :: m Release
    , hostname :: m Text
    , keyboard :: m Keyboard
    , locale :: m Text
    , timezone :: m Text
    , username :: m Text
    , password :: m Text
    , passwordFile :: Maybe FilePath
    , edit :: m Bool
    , reboot :: m Bool
    }
    deriving stock (Generic)

deriving stock instance Show (InstallOpts Maybe)

parseOpts :: (forall a. Parser a -> Parser (m a)) -> Parser (InstallOpts m)
parseOpts f = do
    flake <- parseFlake f
    luks <- parseLuks f
    disk <- f $ parsePath parseAbsFile (long "disk" <> metavar "DISK" <> help "The disk to install to")
    bootMode <-
        f $ parseEnum (long "boot-mode" <> metavar "MODE" <> help "How the system boots, efi or bios (default: detected)")
    format <- f $ parseYesNo "format" "Format the installation disk"
    filesystem <-
        f $ parseEnum (long "filesystem" <> metavar "FS" <> help "The root partition filesystem")
    edition <-
        f $ parseEnum (long "edition" <> metavar "EDITION" <> help "The Trilby edition to install")
    release <- f $ parseEnum (long "release" <> metavar "RELEASE" <> help "The Nixpkgs release to use")
    hostname <- f $ strOption (long "hostname" <> metavar "HOSTNAME" <> help "The hostname to install")
    keyboard <-
        f
            $ strOption
                (long "keyboard" <> metavar "KEYBOARD" <> help "The keyboard layout to use on this system")
            <&> \layout -> Keyboard{layout, variant = Nothing, description = Nothing}
    locale <- f $ strOption (long "locale" <> metavar "LOCALE" <> help "The locale of this system")
    timezone <-
        f $ strOption (long "timezone" <> metavar "TIMEZONE" <> help "The time zone of this system")
    username <-
        f $ strOption (long "username" <> metavar "USERNAME" <> help "The username of the admin user")
    password <-
        f $ strOption (long "password" <> metavar "PASSWORD" <> help "The password of the admin user")
    passwordFile <-
        optional $ strOption (long "password-file" <> metavar "FILE" <> help "Read the password of the admin user from FILE")
    edit <- f $ parseYesNo "edit" "Edit the configuration before installing"
    reboot <- f $ parseYesNo "reboot" "Reboot when done installing"
    pure InstallOpts{..}

validateParsedInstallOpts
    :: (IOE :> es, Log :> es)
    => InstallOpts Maybe
    -> Eff es (InstallOpts Maybe)
validateParsedInstallOpts opts = do
    either (errorExit . fromString) pure do
        for_ opts.hostname validateHostname
        for_ opts.username validateUsername
    case opts.disk of
        Nothing -> pure opts
        Just d -> do
            vd <- fromMaybeM (liftIO exitFailure) $ validateDisk d
            pure $ opts & #disk ?~ vd

validateHostname :: Text -> Either String Text
validateHostname h
    | Text.null h || Text.length h > 63 = Left "hostname must be 1 to 63 characters long"
    | not (Text.all (\c -> isAscii c && (isAlphaNum c || c == '-')) h) =
        Left "hostname may only contain letters, digits, and '-'"
    | Text.head h == '-' || Text.last h == '-' = Left "hostname must not start or end with '-'"
    | otherwise = Right h

validateUsername :: Text -> Either String Text
validateUsername u
    | Text.null u || Text.length u > 32 = Left "username must be 1 to 32 characters long"
    | not (isAsciiLower (Text.head u) || Text.head u == '_') =
        Left "username must start with a lowercase letter or '_'"
    | not (Text.all (\c -> isAsciiLower c || isDigit c || c == '_' || c == '-') u) =
        Left "username may only contain lowercase letters, digits, '_' and '-'"
    | otherwise = Right u

askValidated :: (IOE :> es, Log :> es) => (Text -> Either String Text) -> Eff es Text -> Eff es Text
askValidated validate ask =
    (validate <$> ask) >>= \case
        Right answer -> pure answer
        Left reason -> logAttention_ (fromString reason) >> askValidated validate ask

validateDisk :: (IOE :> es, Log :> es) => Path Abs File -> Eff es (Maybe (Path Abs File))
validateDisk f = do
    status <- liftIO . Exception.try @SomeException . getFileStatus . toFilePath $ f
    if either (const False) isBlockDevice status
        then pure $ Just f
        else do
            logAttention_ $ "Cannot find disk " <> fromPath f
            pure Nothing

askDisk
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Eff es (Path Abs File)
askDisk = do
    disks <-
        mapM (parseAbsFile . fromText)
            =<< shellOutTextLines ["lsblk --raw | grep '\\Wdisk\\W\\+$' | awk '{print \"/dev/\" $1}'"]
    when (null disks) $ errorExit "No disks found"
    fromMaybeM askDisk $ select "Choose installation disk:" disks Nothing fromPath >>= validateDisk

askLuks
    :: (HasCallStack, IOE :> es, Log :> es)
    => Maybe (LuksOpts Maybe)
    -> Eff es (LuksOpts (Eff es))
askLuks opts =
    case opts of
        Nothing -> bool NoLuks UseLuks{luksPassword = askPassword, luksPasswordFile = Nothing} <$> askUseLuks
        Just NoLuks -> pure NoLuks
        Just UseLuks{..} ->
            pure UseLuks{luksPassword = maybe (maybe askPassword readSecretFile luksPasswordFile) pure luksPassword, luksPasswordFile}
  where
    askUseLuks :: (HasCallStack, IOE :> es, Log :> es) => Eff es Bool
    askUseLuks = yesNoButtons "Encrypt the disk with LUKS2?" True
    askPassword :: (HasCallStack, IOE :> es, Log :> es) => Eff es Text
    askPassword = passwordInput "Choose LUKS password:"

detectBootMode :: (FileSystem :> es) => Eff es BootMode
detectBootMode = bool BIOS EFI <$> doesDirExist $(mkAbsDir "/sys/firmware/efi")

readSecretFile :: (HasCallStack, IOE :> es, Log :> es) => FilePath -> Eff es Text
readSecretFile path =
    liftIO (Exception.try @SomeException $ Text.readFile path) >>= \case
        Right t -> pure $ Text.dropWhileEnd (\c -> c == '\r' || c == '\n') t
        Left e -> errorExit $ "Cannot read " <> fromString path <> ": " <> fromString (show e)

askFlake :: (HasCallStack, IOE :> es, Log :> es) => FlakeOpts Maybe -> FlakeOpts (Eff es)
askFlake FlakeOpts{..} =
    FlakeOpts
        { flakeRef
        , copyFlake = maybe (yesNoButtons "Copy flake to /etc/trilby?" True) pure copyFlake
        }

askKeyboard
    :: ( HasCallStack
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => Eff es Keyboard
askKeyboard = do
    allKeyboards <- getAllKeyboards
    let findKeyboard layout variant = find (\k -> k.layout == layout && k.variant == variant) allKeyboards
        exampleKeyboards = mapMaybe (uncurry findKeyboard) [("us", Nothing), ("us", Just "dvorak"), ("de", Nothing)]
    currentKeyboard <- getCurrentKeyboard <&> (>>= \Keyboard{..} -> findKeyboard layout variant)
    searchSelect1
        "Choose keyboard layout:"
        allKeyboards
        exampleKeyboards
        (maybeToList currentKeyboard)
        ishow

askLocale
    :: ( HasCallStack
       , IOE :> es
       , Environment :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Eff es Text
askLocale = do
    currentLocale <- (dropSuffix . fromString =<<) <$> lookupEnv "LC_ALL"
    allLocales <- mapMaybe dropSuffix <$> cmdOutTextLines ["locale", "--all-locales"]
    searchSelect1 "Choose locale:" allLocales exampleLocales (maybeToList currentLocale) id
        <&> addSuffix
  where
    exampleLocales :: [Text]
    exampleLocales = ["en_GB", "de_DE", "es_ES", "fr_FR", "zh_CN"]
    dropSuffix :: Text -> Maybe Text
    dropSuffix t = Text.stripSuffix ".utf8" t <|> Text.stripSuffix ".UTF-8" t
    addSuffix :: Text -> Text
    addSuffix = (<> ".UTF-8")

askTimezone
    :: (HasCallStack, IOE :> es, Reader AppState :> es, Reader Host :> es, TypedProcess :> es, Log :> es)
    => Eff es Text
askTimezone = do
    currentTz <- cmdOutTextFirstLine ["timedatectl", "show", "--property=Timezone", "--value"]
    allTimezones <- cmdOutTextLines ["timedatectl", "list-timezones"]
    searchSelect1 "Choose time zone:" allTimezones exampleTimezones (maybeToList currentTz) id
  where
    exampleTimezones :: [Text]
    exampleTimezones = ["UTC", "CET", "Europe/Amsterdam", "Asia/Singapore", "Japan"]

askOpts
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => InstallOpts Maybe
    -> InstallOpts (Eff es)
askOpts opts =
    InstallOpts
        { flake = askFlake <$> opts.flake
        , luks = askLuks opts.luks
        , disk = maybe askDisk pure opts.disk
        , bootMode = maybe detectBootMode pure opts.bootMode
        , format =
            maybe
                ( yesNoButtons
                    "Erase the selected disk and create new partitions? ALL DATA ON IT WILL BE LOST."
                    False
                )
                pure
                opts.format
        , filesystem = maybe (selectEnum "Choose root partition filesystem:" Nothing) pure opts.filesystem
        , edition = maybe (selectEnum "Choose edition:" Nothing) pure opts.edition
        , release = maybe (selectEnum "Choose release:" Nothing) pure opts.release
        , hostname =
            maybe
                ( askValidated validateHostname
                    $ textInput "Choose hostname:"
                    =<< onHost Localhost Configuration.hostname
                )
                pure
                opts.hostname
        , keyboard = maybe askKeyboard pure opts.keyboard
        , locale = maybe askLocale pure opts.locale
        , timezone = maybe askTimezone pure opts.timezone
        , username =
            maybe (askValidated validateUsername $ textInput "Choose admin username:" "") pure opts.username
        , password = maybe (maybe (passwordInput "Choose admin password:") readSecretFile opts.passwordFile) pure opts.password
        , passwordFile = opts.passwordFile
        , edit = maybe (yesNoButtons "Edit the configuration?" False) pure opts.edit
        , reboot = maybe (yesNoButtons "Reboot system?" True) pure opts.reboot
        }
