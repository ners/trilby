module Trilby.Install.Options where

import Data.Generics.Labels ()
import Data.Text qualified as Text
import Options.Applicative
import System.Posix (getFileStatus, isBlockDevice, userPassword)
import Trilby.Disko.Filesystem
import Trilby.HNix (FlakeRef (..))
import Trilby.Install.Config.Edition
import Trilby.Install.Config.Host (Keyboard (..))
import Trilby.Install.Config.Release
import Trilby.Widgets
import Prelude hiding (error)

data LuksOpts m
    = NoLuks
    | UseLuks {luksPassword :: m Text}
    deriving stock (Generic)

deriving stock instance Eq (LuksOpts Maybe)

deriving stock instance Show (LuksOpts Maybe)

parseLuks :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (m (LuksOpts m))
parseLuks f = do
    f $
        flag' NoLuks (long "no-luks")
            <|> do
                flag' () (long "luks" <> help "Encrypt the disk with LUKS2")
                luksPassword <- f $ strOption (long "luks-password" <> metavar "PASSWORD" <> help "The disk encryption password")
                pure UseLuks{..}

parseKeyboard :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (m Keyboard)
parseKeyboard f = f do
    layout <- strOption (long "keyboard" <> metavar "KEYBOARD" <> help "The keyboard layout to use on this system")
    pure Keyboard{variant = Nothing, ..}

data FlakeOpts m = FlakeOpts {flakeRef :: FlakeRef, copyFlake :: m Bool}
    deriving stock (Generic)

deriving stock instance Show (FlakeOpts Maybe)

parseFlake :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (Maybe (FlakeOpts m))
parseFlake f = do
    optional do
        flakeRef <- strOption (long "flake" <> metavar "FLAKE" <> help "Build the Trilby system from the specified flake")
        copyFlake <- f $ parseYesNo "copy-flake" "Copy the installation flake to /etc/trilby"
        pure FlakeOpts{..}

data InstallOpts m = InstallOpts
    { flake :: Maybe (FlakeOpts m)
    , luks :: m (LuksOpts m)
    , disk :: m (Path Abs File)
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
    format <- f $ parseYesNo "format" "Format the installation disk"
    filesystem <- f $ parseEnum (long "filesystem" <> metavar "FS" <> help "The root partition filesystem")
    edition <- f $ parseEnum (long "edition" <> metavar "EDITION" <> help "The Trilby edition to install")
    release <- f $ parseEnum (long "release" <> metavar "CHANNEL" <> help "The nixpkgs release to use")
    hostname <- f $ strOption (long "hostname" <> metavar "HOSTNAME" <> help "The hostname to install")
    keyboard <- parseKeyboard f
    locale <- f $ strOption (long "locale" <> metavar "LOCALE" <> help "The locale of this system")
    timezone <- f $ strOption (long "timezone" <> metavar "TIMEZONE" <> help "The time zone of this system")
    username <- f $ strOption (long "username" <> metavar "USERNAME" <> help "The username of the admin user")
    password <- f $ strOption (long "password" <> metavar "PASSWORD" <> help "The password of the admin user")
    edit <- f $ parseYesNo "edit" "Edit the configuration before installing"
    reboot <- f $ parseYesNo "reboot" "Reboot when done installing"
    pure InstallOpts{..}

validateParsedInstallOpts :: InstallOpts Maybe -> App (InstallOpts Maybe)
validateParsedInstallOpts opts =
    case opts.disk of
        Nothing -> pure opts
        Just d -> do
            vd <- fromMaybeM (liftIO exitFailure) $ validateDisk d
            pure $ opts & #disk ?~ vd

validateDisk :: Path Abs File -> App (Maybe (Path Abs File))
validateDisk f = do
    status <- liftIO . getFileStatus . toFilePath $ f
    if isBlockDevice status
        then pure $ Just f
        else do
            logError $ "Cannot find disk " <> fromPath f
            pure Nothing

askDisk :: App (Path Abs File)
askDisk = do
    disks <- mapM (parseAbsFile . fromText) . Text.lines =<< shell "lsblk --raw | grep '\\Wdisk\\W\\+$' | awk '{print \"/dev/\" $1}'" empty
    when (null disks) $ errorExit "No disks found"
    fromMaybeM askDisk $ select "Choose installation disk:" disks Nothing fromPath >>= validateDisk

askLuks :: Maybe (LuksOpts Maybe) -> App (LuksOpts App)
askLuks opts =
    case opts of
        Nothing -> bool NoLuks UseLuks{luksPassword = askPassword} <$> askUseLuks
        Just NoLuks -> pure NoLuks
        Just UseLuks{..} -> pure UseLuks{luksPassword = maybe askPassword pure luksPassword}
  where
    askUseLuks = yesNoButtons "Encrypt the disk with LUKS2?" True
    askPassword = passwordInput "Choose LUKS password:"

askFlake :: FlakeOpts Maybe -> FlakeOpts App
askFlake FlakeOpts{..} =
    FlakeOpts
        { flakeRef
        , copyFlake = maybe (yesNoButtons "Copy flake to /etc/trilby?" True) pure copyFlake
        }

askKeyboard :: App Keyboard
askKeyboard = do
    layout <- textInput "Choose keyboard layout:" ""
    pure Keyboard{variant = Nothing, ..}

askLocale :: App Text
askLocale = do
    currentLocale <- firstLine <$> shell "localectl status | sed '/Locale/!d; s/.*LANG=\\(\\S*\\).*/\\1/'" empty
    textInput "Choose locale:" currentLocale

askTimezone :: App Text
askTimezone = do
    currentTz <- firstLine <$> shell "timedatectl show --property=Timezone --value" empty
    allTimezones <- Text.lines <$> shell "timedatectl list-timezones" empty
    fromMaybeM askTimezone $ searchSelect "Choose time zone:" allTimezones [currentTz] id <&> listToMaybe

askOpts :: InstallOpts Maybe -> InstallOpts App
askOpts opts =
    InstallOpts
        { flake = askFlake <$> opts.flake
        , luks = askLuks opts.luks
        , disk = maybe askDisk pure opts.disk
        , format = maybe (yesNoButtons "Format the disk?" True) pure opts.format
        , filesystem = maybe (selectEnum "Choose root partition filesystem:" Nothing) pure opts.filesystem
        , edition = maybe (selectEnum "Choose edition:" Nothing) pure opts.edition
        , release = maybe (selectEnum "Choose release:" Nothing) pure opts.release
        , hostname = maybe (textInput "Choose hostname:" "") pure opts.hostname
        , keyboard = maybe askKeyboard pure opts.keyboard
        , locale = maybe askLocale pure opts.locale
        , timezone = maybe askTimezone pure opts.timezone
        , username = maybe (textInput "Choose admin username:" "") pure opts.username
        , password = maybe (passwordInput "Choose admin password:") pure opts.password
        , edit = maybe (yesNoButtons "Edit the configuration?" False) pure opts.edit
        , reboot = maybe (yesNoButtons "Reboot system?" True) pure opts.reboot
        }
