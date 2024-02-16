{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Update.Options where

import Internal.Prelude
import Options.Applicative

data UpdateAction m
    = Switch
    | Boot {reboot :: m Bool}
    | NoAction
    deriving stock (Generic)

deriving stock instance Eq (UpdateAction Maybe)

data UpdateOpts m = UpdateOpts
    { flakeUpdate :: m Bool
    , action :: m (UpdateAction m)
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
    pure UpdateOpts{..}

askAction :: Maybe (UpdateAction Maybe) -> App (UpdateAction App)
askAction ma =
    ifM
        askSwitch
        (pure Switch)
        (ifM askBoot (pure Boot{..}) (pure NoAction))
  where
    askSwitch = maybe (askYesNo "Switch to the new configuration? (sudo)" True) (pure . (== Switch)) ma
    askBoot = maybe (askYesNo "Apply the new configuration at boot? (sudo)" False) pure isBoot
    reboot = maybe (askYesNo "Reboot to new configuration now? (sudo)" False) pure isReboot
    isBoot = case ma of
        Nothing -> Nothing
        Just Boot{} -> Just True
        _ -> Just False
    isReboot = case ma of
        Just Boot{reboot} -> reboot
        _ -> Nothing

askOpts :: UpdateOpts Maybe -> UpdateOpts App
askOpts opts =
    UpdateOpts
        { flakeUpdate = maybe (pure True) pure opts.flakeUpdate
        , action = askAction opts.action
        }
