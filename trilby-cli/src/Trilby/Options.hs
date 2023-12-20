module Trilby.Options where

import Internal.Prelude
import Options.Applicative hiding (command)
import Trilby.Command (Command, parseCommand)

data Options m = Options
    { verbosity :: m LogLevel
    , command :: Command m
    }

readLogLevel :: String -> Maybe LogLevel
readLogLevel (fmap toLower -> "debug") = Just LevelDebug
readLogLevel (fmap toLower -> "info") = Just LevelInfo
readLogLevel (fmap toLower -> "warn") = Just LevelWarn
readLogLevel (fmap toLower -> "error") = Just LevelError
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
        optional
            $ parseChoiceWith
                showLogLevel
                readLogLevel
                (long "verbosity" <> metavar "LOGLEVEL" <> help "output verbosity")
                [LevelDebug, LevelInfo, LevelWarn, LevelError]
    command <- parseCommand
    pure Options{..}

parseOptionsInfo :: ParserInfo (Options Maybe)
parseOptionsInfo =
    info
        (helper <*> parseOptions)
        (fullDesc <> progDesc "Trilby command-line tool")
