{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Update where

import Control.Applicative (empty)
import Control.Monad.Extra (ifM, whenM)
import Control.Monad.IO.Class (MonadIO)
import Trilby.Options
import Trilby.Util
import Turtle.Prelude hiding (shell, shells)
import Prelude

getAction :: (MonadIO m) => Maybe (UpdateAction Maybe) -> m (UpdateAction m)
getAction ma =
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

getOpts :: forall m. (MonadIO m) => UpdateOpts Maybe -> UpdateOpts m
getOpts opts =
    UpdateOpts
        { action = getAction opts.action
        }

update :: (MonadIO m) => UpdateOpts Maybe -> m ()
update (getOpts -> opts) = do
    cd "/etc/trilby"
    shells "nix flake update" empty
    shells "nixos-rebuild build --flake ." empty
    shells "nvd diff /run/current-system result" empty
    opts.action >>= \case
        Switch -> sudo "nixos-rebuild switch --flake ."
        Boot reboot -> do
            sudo "nixos-rebuild boot --flake . --install-bootloader"
            whenM reboot $ sudo "reboot"
        NoAction -> pure ()
