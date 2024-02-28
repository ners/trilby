{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Trilby.Update (update) where

import Trilby.Update.Options
import Prelude

update :: UpdateOpts Maybe -> App ()
update (askOpts -> opts) = inDir "/etc/trilby" do
    whenM opts.flakeUpdate $ rawCmd_ ["nix", "flake", "update", "--accept-flake-config"]
    withTrace rawCmd_ ["nixos-rebuild", "build", "--flake", ".", "--accept-flake-config"]
    rawCmd_ ["nvd", "diff", "/run/current-system", "result"]
    opts.action >>= \case
        Switch -> (withTrace . asRoot) rawCmd_ ["nixos-rebuild", "switch", "--flake", ".", "--accept-flake-config"]
        Boot{..} -> do
            (withTrace . asRoot) rawCmd_ ["nixos-rebuild", "boot", "--flake", ".", "--accept-flake-config", "--install-bootloader"]
            whenM reboot $ cmd_ ["systemctl", "reboot"]
        NoAction -> pure ()
