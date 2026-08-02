module Trilby.Clean (clean) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Set qualified as Set
import Trilby.BootloaderEntry
import Trilby.Clean.Options
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.Host
import Trilby.Prelude

clean
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => CleanOpts Maybe
    -> Eff es ()
clean (askOpts -> opts) = do
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    for_ configurations \Configuration{..} -> do
        system <- hostSystem host
        whats <- opts.what
        for_ whats \case
            Boot -> boot host system
            Podman -> ssh host cmd_ ["podman", "system", "reset", "--force"]
            Profiles -> ssh host (asRoot $ runProcess_ . proc) ["nix-collect-garbage", "--delete-old"]
            Store -> unless (Profiles `Set.member` whats) $ ssh host cmd_ ["nix-collect-garbage"]

boot
    :: (HasCallStack, Fail :> es, IOE :> es, Reader AppState :> es, TypedProcess :> es, Log :> es)
    => Host
    -> System
    -> Eff es ()
boot host System{kernel = Linux} = do
    getBootloaderEntries host >>= mapM_ \BootloaderEntry{..} ->
        when (type' == Type1 && not isDefault && not isSelected)
            $ ssh host (asRoot $ runProcess_ . proc) ["bootctl", "unlink", id]
    ssh host (asRoot $ runProcess_ . proc) ["bootctl", "cleanup"]
boot _ system = errorExit $ "Cleaning the boot partition is not supported on " <> ishow system.kernel
