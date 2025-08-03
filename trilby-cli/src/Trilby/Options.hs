module Trilby.Options where

import Data.Either.Extra (eitherToMaybe)
import Effectful.Environment (runEnvironment)
import Options.Applicative hiding (command)
import Trilby.Command (Command, parseCommand)
import Trilby.Version qualified as Trilby
import Prelude

data Options m = Options
    { verbosity :: m LogLevel
    , command :: Command m
    }

parseOptions :: Parser (Options Maybe)
parseOptions = do
    let verbosityError = flag' LogAttention $ short 'q' <> long "quiet" <> help "Decrease the logging verbosity level"
    let verbosityInfo = flag' LogInfo $ short 'v' <> long "verbose" <> help "Increase the logging verbosity level"
    let verbosityDebug = flag' LogTrace $ long "debug" <> help "Set the logging verbosity level to 'debug'"
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
    envVerbosity <- liftIO . runEff . runEnvironment $ (eitherToMaybe . readLogLevelEither . fromString =<<) <$> lookupEnv "TRILBY_VERBOSITY"
    pure $ fromMaybe defaultVerbosity $ opts.verbosity <|> envVerbosity
  where
    defaultVerbosity = LogAttention
