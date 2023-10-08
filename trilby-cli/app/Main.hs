module Main where

import Options.Applicative (execParser)
import Trilby.Install (install)
import Trilby.Options
    ( Command (Install, Update)
    , parseCommandInfo
    )
import Trilby.Update (update)
import Prelude

main :: IO ()
main = do
    command <- execParser parseCommandInfo
    case command of
        Update opts -> update opts
        Install opts -> install opts
