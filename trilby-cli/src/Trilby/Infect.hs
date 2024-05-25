module Trilby.Infect where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), copyClosure, nixBuild, trilbyFlake, writeNixFile)
import Trilby.Host
import Trilby.Infect.Options
import Prelude

infect :: InfectOpts Maybe -> App ()
infect (askOpts -> opts) = do
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    for_ configurations \c -> do
        infectLinux opts c

infectLinux :: InfectOpts App -> Configuration -> App ()
infectLinux opts Configuration{..} = do
    kexec <- buildKexec opts
    copyClosure host kexec
    let bin = kexec </> $(mkRelFile "kexec-boot")
    whenM opts.reboot $ ssh host rawCmd_ ["sudo", fromPath bin]

infectDarwin :: InfectOpts App -> Configuration -> App ()
infectDarwin opts Configuration{..} = do
    logError "Not yet implemented"

buildKexec :: (HasCallStack) => InfectOpts App -> App (Path Abs Dir)
buildKexec opts = withTempFile $(mkRelFile "infect.nix") \tmpFile -> do
    trilbyUrl <- trilbyFlake
    edition <- opts.edition
    authorisedKeys <-
        opts.authorisedKeys >>= \case
            AuthorisedKeys keys -> pure keys
            AuthorisedKeysFile file -> liftIO $ Text.lines <$> Text.readFile file
    writeNixFile
        tmpFile
        [nix|
        { trilby ? builtins.getFlake trilbyUrl
        , lib ? trilby.lib
        }:

        let system = lib.trilbySystem {
          trilby = {
            edition = edition;
            hostPlatform = builtins.currentSystem;
          };
          modules = [
            trilby.nixosModules.formats.kexec
            {
              users.users.trilby.openssh.authorizedKeys.keys = authorisedKeys;
            }
          ];
        };
        in system.config.system.build.kexecTree
        |]
    nixBuild (File $ Abs tmpFile) parseAbsDir
