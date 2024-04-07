{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Update.Options where

import Options.Applicative
import Trilby.Widgets
import Prelude
import Trilby.Host

data UpdateAction m
    = Switch
    | Boot {reboot :: m Bool}
    | Test
    | NoAction
    deriving stock (Generic)

deriving stock instance Eq (UpdateAction Maybe)

data UpdateOpts m = UpdateOpts
    { flakeUpdate :: m Bool
    , action :: m (UpdateAction m)
    , hosts :: m (NonEmpty Host)
    }
    deriving stock (Generic)

parseUpdateOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (UpdateOpts m)
parseUpdateOpts f = do
    flakeUpdate <- f $ flag' False (long "no-flake-update" <> help "Do not update the flake lock")
    action <-
        f $
            flag' Switch (long "switch" <> help "Switch to the new configuration")
                <|> do
                    boot <- flag' Boot (long "boot" <> help "Apply the new configuration at boot")
                    reboot <- f $ parseYesNo "reboot" "Reboot to the new configuration"
                    pure $ boot reboot
                <|> flag' NoAction (long "no-action" <> help "Do not apply new configuration")
    hosts <- f . fmap (fromListSafe Localhost) . many $ fromString <$> strArgument (metavar "HOST" <> help "Target host to update")
    pure UpdateOpts{..}

askAction :: Maybe (UpdateAction Maybe) -> App (UpdateAction App)
askAction (Just Switch) = pure Switch
askAction (Just Boot{reboot = askReboot -> reboot}) = pure Boot{..}
askAction (Just Test) = pure Test
askAction (Just NoAction) = pure NoAction
askAction Nothing = do
    let values = [("Switch", 'S'), ("Boot", 'B'), ("Test", 'T'), ("Nothing", 'N')] :: [(Text, Char)]
    selected <- buttons "What now?" values 0 id
    case selected of
        "Switch" -> pure Switch
        "Boot" -> pure Boot{reboot = askReboot Nothing}
        "Test" -> pure Test
        _ -> pure NoAction

askReboot :: Maybe Bool -> App Bool
askReboot = maybe (yesNoButtons "Reboot to new configuration now? (sudo)" False) pure

-- | Convert CLI options to IO options; if an option has been provided on the CLI, we use that, otherwise we either ask the user or default it.
askOpts :: UpdateOpts Maybe -> UpdateOpts App
askOpts opts =
    UpdateOpts
        { flakeUpdate = maybe (pure True) pure opts.flakeUpdate
        , action = askAction opts.action
        , hosts = maybe (pure [Localhost]) pure opts.hosts
        }
