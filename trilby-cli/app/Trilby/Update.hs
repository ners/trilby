{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Trilby.Update where

import Shelly
import Trilby.Options
import Trilby.Util
import Prelude

getOpts :: UpdateOpts Maybe -> UpdateOpts Sh
getOpts opts = do
    let switch = maybe (ask "Switch to configuration?" True) pure opts.switch
    let boot = maybe (ask "Apply the configuration at boot?" False) pure opts.switch
    let reboot = maybe (ask "Reboot to configuration?" False) pure opts.switch
    UpdateOpts{..}

update :: UpdateOpts Maybe -> IO ()
update (getOpts -> opts) = shelly $ peval info $ do
    cd "/etc/trilby"
    cmd "nix" "flake" "update"
    cmd "nixos-rebuild" "build" "--flake" "."
    cmd "nvd" "diff" "/run/current-system" "result"
    whenM opts.switch do
        cmd "sudo" "nixos-rebuild" "switch" "--flake" "."
    whenM opts.boot do
        cmd "sudo" "nixos-rebuild" "boot" "--flake" "."
        whenM opts.reboot do
            cmd "sudo" "reboot"
