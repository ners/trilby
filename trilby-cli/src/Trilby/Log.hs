module Trilby.Log where

import Control.Monad.Logger.CallStack
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import System.Console.ANSI
import Prelude

locStr :: (IsString a) => Loc -> a
locStr loc = fromString $ List.intercalate ":" [loc.loc_filename, show $ fst loc.loc_start, show $ snd loc.loc_start] <> ": "

logStr :: (IsString a) => LogStr -> a
logStr = fromString . Text.unpack . Text.decodeUtf8 . fromLogStr

printError :: Text -> IO ()
printError t = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printWarn :: Text -> IO ()
printWarn t = do
    hSetSGR stderr [SetColor Foreground Vivid Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printInfo :: Text -> IO ()
printInfo t = do
    hSetSGR stderr [SetColor Foreground Dull Cyan]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printDebug :: Text -> IO ()
printDebug t = do
    hSetSGR stderr [SetColor Foreground Dull Magenta]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printFn :: LogLevel -> Text -> IO ()
printFn LevelDebug = printDebug
printFn LevelInfo = printInfo
printFn LevelWarn = printWarn
printFn LevelError = printError
printFn (LevelOther _) = printInfo

printLog :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
printLog verbosity _ _ logLevel | logLevel < verbosity = const $ pure ()
printLog LevelDebug (locStr -> locText) _ logLevel = printFn logLevel . mappend locText . logStr
printLog _ _ _ logLevel = printFn logLevel . logStr

withLog :: LogLevel -> LoggingT m a -> m a
withLog verbosity = flip runLoggingT (printLog verbosity)
