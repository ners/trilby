{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Update where

import Control.Monad.Extra (whenM)
import Trilby.App (App)
import Trilby.Update.Options
import Trilby.Util
import Turtle.Prelude hiding (shell)
import Prelude

update :: UpdateOpts Maybe -> App ()
update (askOpts -> opts) = do
    cd "/etc/trilby"
    whenM opts.flakeUpdate $ cmd_ ["nix", "flake", "update"]
    withTrace cmd_ ["nixos-rebuild", "build", "--flake", "."]
    cmd_ ["nvd", "diff", "/run/current-system", "result"]
    opts.action >>= \case
        Switch -> (withTrace . asRoot) cmd_ ["nixos-rebuild", "switch", "--flake", "."]
        Boot reboot -> do
            (withTrace . asRoot) cmd_ ["nixos-rebuild", "boot", "--flake", ".", "--install-bootloader"]
            whenM reboot $ asRoot cmd_ ["reboot"]
        NoAction -> pure ()
