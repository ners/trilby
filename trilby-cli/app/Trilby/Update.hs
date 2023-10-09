{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Update where

import Control.Applicative (empty)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (MonadIO)
import Trilby.Update.Options
import Trilby.Util
import Turtle.Prelude hiding (shell)
import Prelude

update :: (MonadIO m) => UpdateOpts Maybe -> m ()
update (askOpts -> opts) = do
    cd "/etc/trilby"
    whenM opts.flakeUpdate $ shell_ "nix flake update" empty
    shell_ "nixos-rebuild build --flake ." empty
    shell_ "nvd diff /run/current-system result" empty
    opts.action >>= \case
        Switch -> sudo_ "nixos-rebuild switch --flake ."
        Boot reboot -> do
            sudo_ "nixos-rebuild boot --flake . --install-bootloader"
            whenM reboot $ sudo_ "reboot"
        NoAction -> pure ()
