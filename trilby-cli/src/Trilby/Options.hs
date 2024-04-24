module Trilby.Options where

import Options.Applicative hiding (command)
import Trilby.Command (Command, parseCommand)
import Trilby.Version qualified as Trilby
import UnliftIO.Environment (lookupEnv)
import Prelude

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
    let verbosityError = flag' LevelError $ short 'q' <> long "quiet" <> help "Decrease the logging verbosity level"
    let verbosityInfo = flag' LevelInfo $ short 'v' <> long "verbose" <> help "Increase the logging verbosity level"
    let verbosityDebug = flag' LevelDebug $ long "debug" <> help "Set the logging verbosity level to 'debug'"
    verbosity <- optional $ verbosityError <|> verbosityInfo <|> verbosityDebug
    command <- parseCommand
    simpleVersioner Trilby.fullVersionString
    pure Options{..}

parseOptionsInfo :: ParserInfo (Options Maybe)
parseOptionsInfo =
    info
        (helper <*> parseOptions)
        (fullDesc <> progDesc "Trilby command-line tool")

getVerbosity :: (MonadIO m) => Options Maybe -> m LogLevel
getVerbosity opts = do
    envVerbosity <- (readLogLevel =<<) <$> lookupEnv "TRILBY_VERBOSITY"
    pure $ fromMaybe defaultVerbosity $ opts.verbosity <|> envVerbosity
  where
    defaultVerbosity = LevelWarn
