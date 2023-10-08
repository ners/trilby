{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Options where

import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative
import Trilby.Config.Edition
import Prelude

data Command m
    = Update (UpdateOpts m)
    | Install (InstallOpts m)
    deriving stock (Generic)

deriving stock instance Eq (Command Maybe)

deriving stock instance Show (Command Maybe)

parseCommandInfo :: ParserInfo (Command Maybe)
parseCommandInfo =
    info
        (helper <*> parseCommand)
        (fullDesc <> progDesc "Trilby command-line tool")

parseCommand :: Parser (Command Maybe)
parseCommand = hsubparser (parseUpdate <> parseInstall)

data UpdateAction m
    = Switch
    | Boot {reboot :: m Bool}
    | NoAction
    deriving stock (Generic)

deriving stock instance Eq (UpdateAction Maybe)

deriving stock instance Show (UpdateAction Maybe)

data UpdateOpts m = UpdateOpts
    { flakeUpdate :: m Bool
    , action :: m (UpdateAction m)
    }
    deriving stock (Generic)

deriving stock instance Eq (UpdateOpts Maybe)

deriving stock instance Show (UpdateOpts Maybe)

parseYesNo :: String -> String -> (Parser Bool -> Parser (m Bool)) -> Parser (m Bool)
parseYesNo yesLong yesHelp f = f $ flag' True (long yesLong <> help yesHelp) <|> flag' False (long noLong)
  where
    noLong = "no-" <> yesLong

parseUpdateOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (UpdateOpts m)
parseUpdateOpts f = do
    flakeUpdate <- f $ flag' False (long "no-flake-update" <> help "Do not update the flake lock")
    action <-
        f $
            flag' Switch (long "switch" <> help "Switch to the new configuration")
                <|> do
                    boot <- flag' Boot (long "boot" <> help "Apply the new configuration at boot")
                    reboot <- parseYesNo "reboot" "Reboot to the new configuration" f
                    pure $ boot reboot
                <|> flag' NoAction (long "no-action" <> help "Do not apply new configuration")
    pure UpdateOpts{..}

parseUpdate :: Mod CommandFields (Command Maybe)
parseUpdate = command "update" $ info (Update <$> parseUpdateOpts optional) (progDesc "update desc")

data LuksOpts m
    = NoLuks
    | UseLuks {luksPassword :: m Text}

deriving stock instance Eq (LuksOpts Maybe)

deriving stock instance Show (LuksOpts Maybe)

data InstallOpts m = InstallOpts
    { efi :: m Bool
    , luks :: m (LuksOpts m)
    , disk :: m Text
    , format :: m Bool
    , edition :: m Edition
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
    edition <- f $ flag' Workstation (long "workstation" <> help "install Trilby Workstation edition") <|> flag' Server (long "server" <> help "install Trilby Server edition")
    hostname <- f $ strOption (long "host" <> metavar "HOSTNAME" <> help "the hostname to install")
    username <- f $ strOption (long "username" <> metavar "USERNAME" <> help "the username of the admin user")
    password <- f $ strOption (long "password" <> metavar "PASSWORD" <> help "the password of the admin user")
    reboot <- parseYesNo "reboot" "reboot when done installing" f
    pure InstallOpts{..}

parseInstall :: Mod CommandFields (Command Maybe)
parseInstall = command "install" $ info (Install <$> parseInstallOpts optional) (progDesc "install desc")
