{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trilby.Update.Options where

import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Options.Applicative
import Trilby.Util
import Prelude

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

askAction :: (MonadIO m) => Maybe (UpdateAction Maybe) -> m (UpdateAction m)
askAction ma =
    ifM
        askSwitch
        (pure Switch)
        (ifM askBoot (pure Boot{..}) (pure NoAction))
  where
    askSwitch = maybe (ask "Switch to the new configuration? (sudo)" True) (pure . (== Switch)) ma
    askBoot = maybe (ask "Apply the new configuration at boot? (sudo)" False) pure isBoot
    reboot = maybe (ask "Reboot to new configuration now? (sudo)" False) pure isReboot
    isBoot = case ma of
        Nothing -> Nothing
        Just Boot{} -> Just True
        _ -> Just False
    isReboot = case ma of
        Just Boot{reboot} -> reboot
        _ -> Nothing

askOpts :: forall m. (MonadIO m) => UpdateOpts Maybe -> UpdateOpts m
askOpts opts =
    UpdateOpts
        { flakeUpdate = maybe (pure True) pure opts.flakeUpdate
        , action = askAction opts.action
        }
