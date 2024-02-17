module Main where

import Control.Monad.Logger
import Data.Maybe (fromMaybe)
import Log (withLog)
import Options.Applicative (execParser)
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
        pure
            AppState
                { verbosity = fromMaybe LevelWarn opts.verbosity
                }
    withLog state.verbosity $
        runApp state $
            case opts.command of
                Update o -> update o
                Install o -> install =<< validateParsedInstallOpts o
