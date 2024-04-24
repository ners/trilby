module Trilby.Infect where

import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Trilby.Configuration (Configuration (..))
import Trilby.Configuration qualified as Configuration
import Trilby.HNix (FileOrFlake (..), currentSystem, nixBuild, trilbyFlake, writeNixFile)
import Trilby.Host
import Trilby.Infect.Options
import Turtle ((</>))
import Turtle.Bytes qualified as Turtle
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
        whenM opts.reboot $ ssh host rawCmd_ ["sudo", fromString binPath]

buildKexec :: InfectOpts App -> App FilePath
buildKexec opts = withSystemTempFile "trilby-infect-.nix" $ \tmpFile handle -> do
    hClose handle
    trilbyUrl <- trilbyFlake
    edition <- opts.edition
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
          ];
        };
        in system.config.system.build.kexec_tarball
        |]
    resultDir <- nixBuild $ File tmpFile
    system <- currentSystem
    pure $ resultDir </> "tarball" </> ("nixos-system-" <> fromText system <> ".tar.xz")

extractKexec :: Host -> FilePath -> App FilePath
extractKexec host srcPath = withSystemTempDirectory "trilby-infect-" $ \dstDir -> do
    case host of
        Localhost -> cmd_ ["tar", "-xf", fromString srcPath, "-C", fromString dstDir]
        Host{} ->
            let command =
                    Text.unwords . mconcat $
                        [ ["ssh", ishow host]
                        , ["'"]
                        , ["mkdir", "-p", fromString dstDir]
                        , ["&&"]
                        , ["tar", "-xf", "-", "-C", fromString dstDir]
                        , ["'"]
                        ]
             in Turtle.input srcPath & byteShells command
    pure dstDir
