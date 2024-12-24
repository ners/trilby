module Main where

import Options.Applicative (execParser)
import Trilby.App
import Trilby.Command
import Trilby.Infect (infect)
import Trilby.Install (install)
import Trilby.Install.Options (validateParsedInstallOpts)
import Trilby.Log (withLog)
import Trilby.Options
import Trilby.Update (update)
import Prelude
import Trilby.Setup (setup)


main :: IO ()
main = withSystemTempDir "trilby" \tmpDir -> do
    opts <- execParser parseOptionsInfo
    verbosity <- getVerbosity opts
    commandCache <- newTVarIO mempty
    let state = AppState{..}
    withLog state.verbosity do
        runApp state do
            setup
            case opts.command of
                Update o -> update o
                Install o -> install =<< validateParsedInstallOpts o
                Infect o -> infect o
