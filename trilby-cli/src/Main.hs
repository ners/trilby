module Main where

import Options.Applicative (execParser)
import Trilby.App
import Trilby.Clean (clean)
import Trilby.Command
import Trilby.Infect (infect)
import Trilby.Install (install)
import Trilby.Install.Options (validateParsedInstallOpts)
import Trilby.Options
import Trilby.Setup (setup)
import Trilby.Update (update)
import Prelude

main :: IO ()
main = do
    opts <- execParser parseOptionsInfo
    verbosity <- getVerbosity opts
    runApp verbosity do
        setup
        case opts.command of
            Clean o -> clean o
            Infect o -> infect o
            Install o -> install =<< validateParsedInstallOpts o
            Update o -> update o
