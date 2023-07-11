{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Update where

import Control.Applicative (empty)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (MonadIO)
import Trilby.Options
import Trilby.Util
import Turtle.Prelude hiding (shell, shells)
import Prelude

getOpts :: (MonadIO m) => UpdateOpts Maybe -> UpdateOpts m
getOpts opts =
    UpdateOpts
        { switch = maybe (ask "Switch to configuration? (sudo)" True) pure opts.switch
        , boot = maybe (ask "Apply the configuration at boot? (sudo)" False) pure opts.boot
        , reboot = maybe (ask "Reboot to configuration? (sudo)" False) pure opts.reboot
        }

update :: MonadIO m => UpdateOpts Maybe -> m ()
update (getOpts -> opts) = do
    cd "/etc/trilby"
    shells "nix flake update" empty
    shells "nixos-rebuild build --flake ." empty
    shells "nvd diff /run/current-system result" empty
    whenM opts.switch do
        sudo "result/bin/switch-to-configuration switch"
    whenM opts.boot do
        sudo "result/bin/switch-to-configuration boot"
        whenM opts.reboot do
            sudo "reboot"
