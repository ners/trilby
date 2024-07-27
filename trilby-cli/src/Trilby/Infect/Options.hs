module Trilby.Infect.Options where

import Data.Text qualified as Text
import Options.Applicative
import Trilby.Host
import Trilby.Install.Config.Edition (Edition)
import Trilby.Widgets
import Prelude

data AuthorisedKeys
    = AuthorisedKeys [Text]
    | AuthorisedKeysFile FilePath

data InfectOpts m = InfectOpts
    { hosts :: m (NonEmpty Host)
    , edition :: m Edition
    , authorisedKeys :: m AuthorisedKeys
    , reboot :: m Bool
    }
    deriving stock (Generic)

parseOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (InfectOpts m)
parseOpts f = do
    hosts <- f . fmap (fromListSafe Localhost) . many $ fromString <$> strArgument (metavar "HOST" <> help "Target host to infect")
    edition <- f $ parseEnum (long "edition" <> metavar "EDITION" <> help "The Trilby edition to infect with")
    reboot <- f $ parseYesNo "reboot" "Reboot when done infecting"
    authorisedKeys <- parseAuthorisedKeys f
    pure InfectOpts{..}

parseAuthorisedKeys :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (m AuthorisedKeys)
parseAuthorisedKeys f = f do
    flag' (AuthorisedKeys []) (long "no-authorised-keys")
        <|> do
            key <- strOption (long "authorised-key" <> metavar "KEY" <> help "Allow access to the infected system with this SSH pubkey")
            pure $ AuthorisedKeys [key]
        <|> do
            file <- strOption (long "authorised-keys-file" <> metavar "FILE" <> help "Allow access to the infected system with these SSH pubkeys")
            pure $ AuthorisedKeysFile file

askOpts :: InfectOpts Maybe -> InfectOpts App
askOpts opts =
    InfectOpts
        { hosts = maybe (pure [Localhost]) pure opts.hosts
        , edition = maybe (selectEnum "Choose edition:" Nothing) pure opts.edition
        , authorisedKeys = maybe askAuthorisedKeys pure opts.authorisedKeys
        , reboot = maybe (yesNoButtons "Reboot system?" True) pure opts.reboot
        }

askAuthorisedKeys :: App AuthorisedKeys
askAuthorisedKeys = AuthorisedKeys . Text.lines <$> multilineTextInput "Authorised SSH keys:" ""
