module Main where

import Effectful.Concurrent.STM (newTVarIO, runConcurrent)
import Effectful.Environment (runEnvironment)
import Effectful.Fail (runFailIO)
import Effectful.FileSystem.Path.IO (runFileSystem)
import Effectful.Process.Typed (runTypedProcess)
import Effectful.Reader.Static (runReader)
import Effectful.Temporary.Path.IO (runTemporary)
import Effectful.Time (runTime)
import Options.Applicative (execParser)
import Trilby.App
import Trilby.Clean (clean)
import Trilby.Command
import Trilby.Host (Host (Localhost), onHost)
import Trilby.Infect (infect)
import Trilby.Install (install)
import Trilby.Install.Options (validateParsedInstallOpts)
import Trilby.Options
import Trilby.Prelude
import Trilby.Setup (ensureNix)
import Trilby.Update (update)
import Trilby.Version qualified as Trilby

main :: IO ()
main = do
    opts <- execParser parseOptionsInfo
    verbosity <- getVerbosity opts
    runEff
        . runLog verbosity
        . (logInfo_ Trilby.fullVersionString >>)
        . runFileSystem
        . runTemporary
        . runTime
        . runTypedProcess
        . runConcurrent
        . runEnvironment
        . runFailIO
        . withSystemTempDir "trilby"
        $ \tmpDir -> do
            commandCache <- newTVarIO mempty
            runReader AppState{..} $ do
                onHost Localhost ensureNix
                case opts.command of
                    Clean o -> clean o
                    Infect o -> infect o
                    Install o -> onHost Localhost $ install =<< validateParsedInstallOpts o
                    Update o -> update o
