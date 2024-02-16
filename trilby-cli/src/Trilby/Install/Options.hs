{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Install.Options where

import Data.Generics.Labels ()
import Internal.Prelude hiding (error)
import Options.Applicative
import System.Posix (getFileStatus, isBlockDevice)
import Trilby.Config.Channel
import Trilby.Config.Edition
import Trilby.Disko.Filesystem
import UnliftIO.Directory (canonicalizePath)

data LuksOpts m
    = NoLuks
    | UseLuks {luksPassword :: m Text}
    deriving stock (Generic)

deriving stock instance Eq (LuksOpts Maybe)

deriving stock instance Show (LuksOpts Maybe)

parseLuks :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (m (LuksOpts m))
parseLuks f = do
    f
        $ flag' NoLuks (long "no-luks")
        <|> do
            flag' () (long "luks" <> help "encrypt the disk with LUKS2")
            luksPassword <- f $ strOption (long "luks-password" <> metavar "PASSWORD" <> help "the disk encryption password")
            pure UseLuks{..}

data FlakeOpts m = FlakeOpts {flakeRef :: !Text, copyFlake :: m Bool}
    deriving stock (Generic)

deriving stock instance Eq (FlakeOpts Maybe)

deriving stock instance Show (FlakeOpts Maybe)

parseFlake :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (Maybe (FlakeOpts m))
parseFlake f = do
    optional do
        flakeRef <- strOption (long "flake" <> metavar "FLAKE" <> help "build the Trilby system from the specified flake")
        copyFlake <- f $ parseYesNo "copy-flake" "copy the installation flake to /etc/trilby"
        pure FlakeOpts{..}

data InstallOpts m = InstallOpts
    { flake :: Maybe (FlakeOpts m)
    , luks :: m (LuksOpts m)
    , disk :: m FilePath
    , format :: m Bool
    , filesystem :: m Format
    , edition :: m Edition
    , channel :: m Channel
    , hostname :: m Text
    , username :: m Text
    , password :: m Text
    , reboot :: m Bool
    }
    deriving stock (Generic)

deriving stock instance Eq (InstallOpts Maybe)

deriving stock instance Show (InstallOpts Maybe)

parseInstallOpts :: (forall a. Parser a -> Parser (m a)) -> Parser (InstallOpts m)
parseInstallOpts f = do
    flake <- parseFlake f
    luks <- parseLuks f
    disk <- f $ strOption (long "disk" <> metavar "DISK" <> help "the disk to install to")
    format <- f $ parseYesNo "format" "format the installation disk"
    filesystem <- f $ parseEnum (long "filesystem" <> metavar "FS" <> help "the root partition filesystem")
    edition <- f $ parseEnum (long "edition" <> metavar "EDITION" <> help "the edition of Trilby to install")
    channel <- f $ parseEnum (long "channel" <> metavar "CHANNEL" <> help "the nixpkgs channel to use")
    hostname <- f $ strOption (long "host" <> metavar "HOSTNAME" <> help "the hostname to install")
    username <- f $ strOption (long "username" <> metavar "USERNAME" <> help "the username of the admin user")
    password <- f $ strOption (long "password" <> metavar "PASSWORD" <> help "the password of the admin user")
    reboot <- f $ parseYesNo "reboot" "reboot when done installing"
    pure InstallOpts{..}

validateParsedInstallOpts :: InstallOpts Maybe -> App (InstallOpts Maybe)
validateParsedInstallOpts opts =
    case opts.disk of
        Nothing -> pure opts
        Just d ->
            validateDisk d >>= \case
                Just vd -> pure $ opts & #disk ?~ fromString vd
                Nothing -> liftIO exitFailure

validateDisk :: FilePath -> App (Maybe FilePath)
validateDisk f = do
    canonical <- canonicalizePath f
    status <- liftIO $ getFileStatus canonical
    if isBlockDevice status
        then pure $ Just canonical
        else do
            $(logError) $ "Cannot find disk " <> fromString f
            pure Nothing

askDisk :: App FilePath
askDisk = do
    disks <-
        lines
            . fromText
            <$> shell "lsblk --raw | grep '\\Wdisk\\W\\+$' | awk '{print \"/dev/\" $1}'" empty
    when (null disks) $ errorExit "No disks found"
    askChoice "Choose installation disk:" disks >>= chooseDisk askDisk
  where
    chooseDisk :: App FilePath -> FilePath -> App FilePath
    chooseDisk f = fromMaybeM f . validateDisk

askLuks :: Maybe (LuksOpts Maybe) -> App (LuksOpts App)
askLuks opts = useLuks <&> bool NoLuks UseLuks{..}
  where
    useLuks = maybe (askYesNo "Encrypt the disk with LUKS2?" True) (const $ pure True) opts
    luksPassword = maybe (askText "Choose LUKS password:") pure (opts >>= (.luksPassword))

askFlake :: FlakeOpts Maybe -> FlakeOpts App
askFlake FlakeOpts{..} =
    FlakeOpts
        { flakeRef
        , copyFlake = maybe (askYesNo "Copy flake to /etc/trilby?" True) pure copyFlake
        }

askOpts :: InstallOpts Maybe -> InstallOpts App
askOpts opts = do
    InstallOpts
        { flake = askFlake <$> opts.flake
        , luks = askLuks opts.luks
        , disk = maybe askDisk pure opts.disk
        , format = maybe (askYesNo "Format the disk?" True) pure opts.format
        , filesystem = maybe (askEnum "Choose root partition filesystem:") pure opts.filesystem
        , edition = maybe (askEnum "Choose edition:") pure opts.edition
        , channel = maybe (askEnum "Choose channel:") pure opts.channel
        , hostname = maybe (askText "Choose hostname:") pure opts.hostname
        , username = maybe (askText "Choose admin username:") pure opts.username
        , password = maybe (askText "Choose admin password:") pure opts.password
        , reboot = maybe (askYesNo "Reboot system?" True) pure opts.reboot
        }
