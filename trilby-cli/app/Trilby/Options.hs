{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Options where

import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative
import Trilby.Config (Edition (..))
import Prelude

data Command m
    = Update (UpdateOpts m)
    | Install (InstallOpts m)
    deriving stock (Generic)

deriving stock instance Show (Command Maybe)

parseCommandInfo :: ParserInfo (Command Maybe)
parseCommandInfo =
    info
        (helper <*> parseCommand)
        (fullDesc <> progDesc "Trilby command-line tool")

parseCommand :: Parser (Command Maybe)
parseCommand = hsubparser (parseUpdate <> parseInstall)

data UpdateOpts m = UpdateOpts
    { switch :: m Bool
    , boot :: m Bool
    , reboot :: m Bool
    }
    deriving stock (Generic)

deriving stock instance Show (UpdateOpts Maybe)

parseYesNo :: String -> String -> (Parser Bool -> Parser (m Bool)) -> Parser (m Bool)
parseYesNo yesLong yesHelp f = f $ flag' True (long yesLong <> help yesHelp) <|> flag' False (long $ "no-" <> yesLong)

parseUpdateOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (UpdateOpts m)
parseUpdateOpts f = do
    switch <- parseYesNo "switch" "Switch to the new configuration" f
    boot <- parseYesNo "boot" "Apply the new configuration at boot" f
    reboot <- parseYesNo "reboot" "Reboot to the new configuration" f
    pure UpdateOpts{..}

parseUpdate :: Mod CommandFields (Command Maybe)
parseUpdate = command "update" $ info (Update <$> parseUpdateOpts optional) (progDesc "update desc")

data InstallOpts m = InstallOpts
    { efi :: m Bool
    , luks :: m Bool
    , disk :: m Text
    , format :: m Bool
    , edition :: m Edition
    , host :: m Text
    , username :: m Text
    , reboot :: m Bool
    }
    deriving stock (Generic)

deriving stock instance Show (InstallOpts Maybe)

parseInstallOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (InstallOpts m)
parseInstallOpts f = do
    efi <- f $ flag' True (long "efi" <> help "use EFI boot") <|> flag' False (long "bios" <> help "use BIOS boot")
    luks <- parseYesNo "luks" "Encrypt disk with LUKS2" f
    disk <- f $ strOption (long "disk" <> metavar "DISK" <> help "the disk to install to")
    format <- parseYesNo "format" "format the installation disk" f
    edition <- f $ flag' Workstation (long "workstation" <> help "install Trilby Workstation edition") <|> flag' Server (long "server" <> help "install Trilby Server edition")
    host <- f $ strOption (long "host" <> metavar "HOST" <> help "the hostname to install")
    username <- f $ strOption (long "username" <> metavar "USERNAME" <> help "the username of the admin user")
    reboot <- parseYesNo "reboot" "Reboot when done installing" f
    pure InstallOpts{..}

parseInstall :: Mod CommandFields (Command Maybe)
parseInstall = command "install" $ info (Install <$> parseInstallOpts optional) (progDesc "install desc")
