{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Install.Options where

import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Options.Applicative
import System.Posix (getFileStatus, isBlockDevice)
import Trilby.Config.Channel
import Trilby.Config.Edition
import Trilby.Disko.Filesystem
import Trilby.Util
import Prelude hiding (error)

data LuksOpts m
    = NoLuks
    | UseLuks {luksPassword :: m Text}
    deriving stock (Generic)

deriving stock instance Eq (LuksOpts Maybe)

deriving stock instance Show (LuksOpts Maybe)

data InstallOpts m = InstallOpts
    { efi :: m Bool
    , luks :: m (LuksOpts m)
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
    efi <- f $ flag' True (long "efi" <> help "use EFI boot") <|> flag' False (long "bios" <> help "use BIOS boot")
    luks <- parseLuks f
    disk <- f $ strOption (long "disk" <> metavar "DISK" <> help "the disk to install to")
    format <- parseYesNo "format" "format the installation disk" f
    filesystem <- parseEnum (long "filesystem" <> metavar "FS" <> help "the root partition filesystem") f
    edition <- parseEnum (long "edition" <> metavar "EDITION" <> help "the edition of Trilby to install") f
    channel <- parseEnum (long "channel" <> metavar "CHANNEL" <> help "the nixpkgs channel to use") f
    hostname <- f $ strOption (long "host" <> metavar "HOSTNAME" <> help "the hostname to install")
    username <- f $ strOption (long "username" <> metavar "USERNAME" <> help "the username of the admin user")
    password <- f $ strOption (long "password" <> metavar "PASSWORD" <> help "the password of the admin user")
    reboot <- parseYesNo "reboot" "reboot when done installing" f
    pure InstallOpts{..}

askDisk :: (MonadIO m) => m Text
askDisk = do
    disks <- fmap ("/dev/" <>) . Text.lines <$> inshellstrict "lsblk --raw | grep '\\W\\+disk\\W\\+$' | awk '{print $1}'" empty
    disk <- askChoice "Choose installation disk:" disks
    diskStatus <- liftIO $ getFileStatus $ Text.unpack disk
    if isBlockDevice diskStatus
        then pure disk
        else do
            logerror $ "Cannot find disk: " <> disk
            askDisk

askLuks :: (MonadIO m) => Maybe (LuksOpts Maybe) -> m (LuksOpts m)
askLuks opts = ifM askLuks (pure UseLuks{..}) (pure NoLuks)
  where
    askLuks = maybe (ask "Encrypt the disk with LUKS2?" True) (const $ pure True) opts
    luksPassword = maybe (prompt "Choose LUKS password:") pure (opts >>= (.luksPassword))

askOpts :: forall m. (MonadIO m) => InstallOpts Maybe -> InstallOpts m
askOpts opts = do
    InstallOpts
        { efi = maybe (ask "Use EFI boot?" True) pure opts.efi
        , luks = askLuks opts.luks
        , disk = maybe askDisk pure opts.disk
        , format = maybe (ask "Format the disk?" True) pure opts.format
        , filesystem = maybe (askEnum "Choose root partition filesystem:") pure opts.filesystem
        , edition = maybe (askEnum "Choose edition:") pure opts.edition
        , channel = maybe (askEnum "Choose channel:") pure opts.channel
        , hostname = maybe (prompt "Choose hostname:") pure opts.hostname
        , username = maybe (prompt "Choose admin username:") pure opts.username
        , password = maybe (prompt "Choose admin password:") pure opts.password
        , reboot = maybe (ask "Reboot system?" True) pure opts.reboot
        }
