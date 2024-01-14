{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Update where

import Internal.Prelude
import Trilby.Update.Options
import Turtle.Prelude hiding (shell)

update :: UpdateOpts Maybe -> App ()
update (askOpts -> opts) = do
    cd "/etc/trilby"
    whenM opts.flakeUpdate $ rawCmd_ ["nix", "flake", "update", "--accept-flake-config"]
    withTrace rawCmd_ ["nixos-rebuild", "build", "--flake", ".", "--accept-flake-config"]
    rawCmd_ ["nvd", "diff", "/run/current-system", "result"]
    opts.action >>= \case
        Switch -> (withTrace . asRoot) rawCmd_ ["nixos-rebuild", "switch", "--flake", ".", "--accept-flake-config"]
        Boot reboot -> do
            (withTrace . asRoot) rawCmd_ ["nixos-rebuild", "boot", "--flake", ".", "--accept-flake-config", "--install-bootloader"]
            whenM reboot $ asRoot cmd_ ["reboot"]
        NoAction -> pure ()
