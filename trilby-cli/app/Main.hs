{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Options.Applicative (execParser)
import Trilby.Install
import Trilby.Options
import Trilby.Update
import Prelude

main :: IO ()
main = do
    command <- execParser parseCommandInfo
    case command of
        Update opts -> update opts
        Install opts -> install opts
