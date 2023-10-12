module Main where

import Control.Monad.Extra (whenM)
import Control.Monad.Logger
import Data.List qualified as List
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative (execParser)
import Trilby.App
import Trilby.Command
import Trilby.Install (install)
import Trilby.Logger (ErrorLoggingT (runErrorLoggingT))
import Trilby.Options
import Trilby.Update (update)
import Trilby.Util
import UnliftIO (MonadIO (liftIO), atomically, newTVarIO, readTVarIO, writeTVar)
import Prelude

app :: AppState -> ErrorLoggingT IO ()
app state = do
    runApp state do
        opts <- liftIO $ execParser parseOptionsInfo
        mapM_ (atomically . writeTVar state.verbosity) opts.verbosity
        case opts.command of
            Update o -> update o
            Install o -> install o

printLog :: IO LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
printLog verbosity loc _logSource logLevel (Text.decodeUtf8 . fromLogStr -> logText) =
    whenM
        ((logLevel >=) <$> verbosity)
        case logLevel of
            LevelDebug ->
                printDebug $
                    Text.concat
                        [ fromString $
                            List.intercalate
                                ":"
                                [ loc.loc_filename
                                , show $ fst loc.loc_start
                                , show $ snd loc.loc_start
                                ]
                        , ": "
                        , logText
                        ]
            LevelInfo -> printInfo logText
            LevelWarn -> printWarn logText
            LevelError -> printError logText
            LevelOther _ -> printInfo logText

main :: IO ()
main = do
    verbosity <- newTVarIO LevelWarn
    let state = AppState{..}
    runLoggingT (runErrorLoggingT $ app state) (printLog $ readTVarIO state.verbosity)
