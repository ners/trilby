module Trilby.Infect.Options where

import Options.Applicative
import Trilby.Host
import Trilby.Install.Config.Edition (Edition)
import Trilby.Widgets
import Prelude

data InfectOpts m = InfectOpts
    { hosts :: m (NonEmpty Host)
    , edition :: m Edition
    , reboot :: m Bool
    }
    deriving stock (Generic)

parseOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (InfectOpts m)
parseOpts f = do
    hosts <- f . fmap (fromListSafe Localhost) . many $ fromString <$> strArgument (metavar "HOST" <> help "Target host to infect")
    edition <- f $ parseEnum (long "edition" <> metavar "EDITION" <> help "The Trilby edition to infect with")
    reboot <- f $ parseYesNo "reboot" "Reboot when done infecting"
    pure InfectOpts{..}

askOpts :: InfectOpts Maybe -> InfectOpts App
askOpts opts =
    InfectOpts
        { hosts = maybe (pure [Localhost]) pure opts.hosts
        , edition = maybe (selectEnum "Choose edition:" Nothing) pure opts.edition
        , reboot = maybe (yesNoButtons "Reboot system?" True) pure opts.reboot
        }
