module Trilby.Clean (clean) where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Set qualified as Set
import Trilby.BootloaderEntry
import Trilby.Clean.Options
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.Host
import Trilby.System
import Prelude

clean :: CleanOpts Maybe -> App ()
clean (askOpts -> opts) = do
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    for_ configurations \Configuration{..} -> do
        system <- hostSystem host
        whats <- opts.what
        for_ whats \case
            Boot -> boot host system
            Podman -> ssh host cmd_ ["sudo", "podman", "system", "reset", "--force"]
            Profiles -> ssh host cmd_ ["sudo", "nix-collect-garbage", "--delete-old"]
            Store -> unless (Profiles `Set.member` whats) $ ssh host cmd_ ["nix-collect-garbage"]

boot :: Host -> System -> App ()
boot host System{kernel = Linux} = do
    getBootloaderEntries host >>= mapM_ \BootloaderEntry{..} ->
        when (type' == Type1 && not isDefault && not isSelected) $
            ssh host cmd_ ["sudo", "bootctl", "unlink", id]
    ssh host cmd_ ["sudo", "bootctl", "cleanup"]
boot _ system = errorExit $ "Cleaning the boot partition is not supported on " <> ishow system.kernel
