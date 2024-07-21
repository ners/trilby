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

main :: IO ()
main = do
    opts <- execParser parseOptionsInfo
    verbosity <- getVerbosity opts
    hostname <- fromString . head . lines <$> readFile "/etc/hostname"
    let state = AppState{..}
    withLog state.verbosity do
        runApp state do
            case opts.command of
                Update o -> update o
                Install o -> install =<< validateParsedInstallOpts o
                Infect o -> infect o
