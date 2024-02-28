module Main where

import Control.Monad.Logger
import Data.Maybe (fromMaybe)
import Log (withLog)
import Options.Applicative (execParser, (<|>))
import System.Environment (lookupEnv)
import Trilby.App
import Trilby.Command
import Trilby.Install (install)
import Trilby.Install.Options (validateParsedInstallOpts)
import Trilby.Options
import Trilby.Update (update)
import Prelude

main :: IO ()
main = do
    opts <- execParser parseOptionsInfo
    state <- do
        defaultVerbosity <- (readLogLevel =<<) <$> lookupEnv "TRILBY_VERBOSITY"
        let verbosity = fromMaybe LevelWarn $ opts.verbosity <|> defaultVerbosity
        pure AppState{..}
    withLog state.verbosity do
        runApp state do
            case opts.command of
                Update o -> update o
                Install o -> install =<< validateParsedInstallOpts o
