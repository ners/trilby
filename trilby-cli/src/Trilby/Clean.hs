module Trilby.Clean (clean) where

import Data.Set qualified as Set
import Trilby.BootloaderEntry
import Trilby.Clean.Options
import Trilby.Host
import Trilby.Prelude

clean
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => CleanOpts Maybe
    -> Eff es ()
clean (askOpts -> opts) =
    opts.hosts >>= mapM_ \host -> onHost host do
        system <- hostSystem
        whats <- inject opts.what
        for_ whats \case
            Boot -> boot system
            Podman -> cmd_ ["podman", "system", "reset", "--force"]
            Profiles -> asRoot (withHost $ runProcess_ . proc) ["nix-collect-garbage", "--delete-old"]
            Store -> unless (Profiles `Set.member` whats) $ cmd_ ["nix-collect-garbage"]

boot
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => System
    -> Eff es ()
boot System{kernel = Linux} = do
    getBootloaderEntries >>= mapM_ \BootloaderEntry{..} ->
        when (type' == Type1 && not isDefault && not isSelected)
            $ asRoot (withHost $ runProcess_ . proc) ["bootctl", "unlink", id]
    asRoot (withHost $ runProcess_ . proc) ["bootctl", "cleanup"]
boot system = errorExit $ "Cleaning the boot partition is not supported on " <> ishow system.kernel
