module Trilby.Infect where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), nixBuild, trilbyFlake, writeNixFile)
import Trilby.Host
import Trilby.Infect.Options
import Turtle ((</>))
import Prelude

infect :: InfectOpts Maybe -> App ()
infect (askOpts -> opts) = do
    configurations <- mapM Configuration.fromHost . NonEmpty.nubOrd =<< opts.hosts
    kexec <- buildKexec opts
    for_ configurations $ \Configuration{..} -> do
        targetPath <- extractKexec host kexec
        let binPath =
                foldr1 @[]
                    (</>)
                    [ targetPath
                    , "kexec_trilby"
                    , "bin"
                    , "kexec-trilby"
                    ]
        whenM opts.reboot $ ssh host rawCmd_ [fromString binPath]

buildKexec :: (HasCallStack) => InfectOpts App -> App FilePath
buildKexec opts = withSystemTempFile "trilby-infect-.nix" $ \tmpFile handle -> do
    hClose handle
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
    resultDir <- nixBuild $ File tmpFile
    pure $ resultDir </> "tarball" </> "trilby-kexec.tar"

extractKexec :: (HasCallStack) => Host -> FilePath -> App FilePath
extractKexec host srcPath = do
    let dstDir = "/" :: FilePath
    case host of
        Localhost -> cmd_ ["tar", "-xf", fromString srcPath, "-C", fromString dstDir]
        Host{} ->
            let command =
                    Text.unwords . mconcat $
                        [ ["cat", fromString srcPath]
                        , ["|"]
                        , ["ssh", ishow host]
                        , ["'"]
                        , ["tar", "-xf", "-", "-C", fromString dstDir]
                        , ["'"]
                        ]
             in void $ shell command empty
    pure dstDir
