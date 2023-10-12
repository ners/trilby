module Trilby.Options where

import Control.Monad.Logger (LogLevel (..))
import Data.Char (toLower)
import Options.Applicative hiding (command)
import Trilby.Command (Command, parseCommand)
import Trilby.Util
import Prelude

data Options m = Options
    { verbosity :: m LogLevel
    , command :: Command m
    }

readLogLevel :: String -> Maybe LogLevel
readLogLevel "debug" = Just LevelDebug
readLogLevel "info" = Just LevelInfo
readLogLevel "warn" = Just LevelWarn
readLogLevel "error" = Just LevelError
readLogLevel _ = Nothing

showLogLevel :: LogLevel -> String
showLogLevel LevelDebug = "Debug"
showLogLevel LevelInfo = "Info"
showLogLevel LevelWarn = "Warn"
showLogLevel LevelError = "Error"
showLogLevel LevelOther{} = "Other"

parseOptions :: Parser (Options Maybe)
parseOptions = do
    verbosity <-
        optional $
            parseChoiceWith
                showLogLevel
                (readLogLevel . fmap toLower)
                (long "verbosity" <> metavar "LOGLEVEL" <> help "output verbosity")
                [LevelDebug, LevelInfo, LevelWarn, LevelError]
    command <- parseCommand
    pure Options{..}

parseOptionsInfo :: ParserInfo (Options Maybe)
parseOptionsInfo =
    info
        (helper <*> parseOptions)
        (fullDesc <> progDesc "Trilby command-line tool")
