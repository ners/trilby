{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Install.Options where

import Control.Monad.Extra (ifM)
import Control.Monad.Logger (logError, logInfo)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Options.Applicative
import System.Posix (getFileStatus, isBlockDevice)
import Trilby.App (App)
import Trilby.Config.Channel
import Trilby.Config.Edition
import Trilby.Disko.Filesystem
import Trilby.Util
import UnliftIO (MonadIO (liftIO))
import Prelude hiding (error)

data LuksOpts m
    = NoLuks
    | UseLuks {luksPassword :: m Text}
    deriving stock (Generic)

deriving stock instance Eq (LuksOpts Maybe)

deriving stock instance Show (LuksOpts Maybe)

data InstallOpts m = InstallOpts
    { luks :: m (LuksOpts m)
    , disk :: m Text
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

parseLuks :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (m (LuksOpts m))
parseLuks f = do
    f $
        flag' NoLuks (long "no-luks")
            <|> do
                flag' () (long "luks" <> help "encrypt the disk with LUKS2")
                luksPassword <- f $ strOption (long "luks-password" <> metavar "PASSWORD" <> help "the disk encryption password")
                pure UseLuks{..}

parseInstallOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (InstallOpts m)
parseInstallOpts f = do
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

askDisk :: App Text
askDisk = do
    disks <-
        Text.lines
            <$> shell "lsblk --raw | grep '\\Wdisk\\W\\+$' | awk '{print \"/dev/\" $1}'" empty
    case disks of
        [] -> errorExit "No disks found"
        [d] -> do
            $(logInfo) $ "Using disk: " <> d
            pure d
        _ -> do
            disk <- askChoice "Choose installation disk:" disks
            diskStatus <- liftIO . getFileStatus $ fromText disk
            if isBlockDevice diskStatus
                then pure disk
                else do
                    $(logError) $ "Cannot find disk: " <> disk
                    askDisk

askLuks :: Maybe (LuksOpts Maybe) -> App (LuksOpts App)
askLuks opts = ifM useLuks (pure UseLuks{..}) (pure NoLuks)
  where
    useLuks = maybe (askYesNo "Encrypt the disk with LUKS2?" True) (const $ pure True) opts
    luksPassword = maybe (prompt "Choose LUKS password:") pure (opts >>= (.luksPassword))

askOpts :: InstallOpts Maybe -> InstallOpts App
askOpts opts = do
    InstallOpts
        { luks = askLuks opts.luks
        , disk = maybe askDisk pure opts.disk
        , format = maybe (askYesNo "Format the disk?" True) pure opts.format
        , filesystem = maybe (askEnum "Choose root partition filesystem:") pure opts.filesystem
        , edition = maybe (askEnum "Choose edition:") pure opts.edition
        , channel = maybe (askEnum "Choose channel:") pure opts.channel
        , hostname = maybe (prompt "Choose hostname:") pure opts.hostname
        , username = maybe (prompt "Choose admin username:") pure opts.username
        , password = maybe (prompt "Choose admin password:") pure opts.password
        , reboot = maybe (askYesNo "Reboot system?" True) pure opts.reboot
        }
