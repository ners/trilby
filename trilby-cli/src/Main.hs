module Main where

import Options.Applicative (execParser)
import Trilby.App
import Trilby.Clean (clean)
import Trilby.Command
import Trilby.Infect (infect)
import Trilby.Install (install)
import Trilby.Install.Options (askLocale, validateParsedInstallOpts, askTimezone, askKeyboard)
import Trilby.Log (withLog)
import Trilby.Options
import Trilby.Setup (setup)
import Trilby.Update (update)
import Prelude

main :: IO ()
main = withSystemTempDir "trilby" \tmpDir -> do
    opts <- execParser parseOptionsInfo
    verbosity <- getVerbosity opts
    commandCache <- newTVarIO mempty
    let state = AppState{..}
    withLog state.verbosity do
        runApp state do
            liftIO . print =<< askTimezone
            liftIO . print =<< askLocale
            liftIO . print =<< askKeyboard
            setup
            case opts.command of
                Clean o -> clean o
                Infect o -> infect o
                Install o -> install =<< validateParsedInstallOpts o
                Update o -> update o
