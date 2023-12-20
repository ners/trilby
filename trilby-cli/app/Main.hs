module Main where

import Control.Monad.Logger
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Options.Applicative (execParser)
import System.Console.ANSI
import System.IO (stderr)
import Trilby.App
import Trilby.Command
import Trilby.Install (install)
import Trilby.Install.Options (validateParsedInstallOpts)
import Trilby.Options
import Trilby.Update (update)
import Prelude

locStr :: (IsString a) => Loc -> a
locStr loc = fromString $ List.intercalate ":" [loc.loc_filename, show $ fst loc.loc_start, show $ snd loc.loc_start]

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

printLog :: LogLevel -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
printLog verbosity _ _ logLevel | logLevel < verbosity = const $ pure ()
printLog _ (locStr -> locText) _ LevelDebug = printDebug . mappend (locText <> ": ") . logStr
printLog _ _ _ LevelInfo = printInfo . logStr
printLog _ _ _ LevelWarn = printWarn . logStr
printLog _ _ _ LevelError = printError . logStr
printLog _ _ _ (LevelOther _) = printInfo . logStr

main :: IO ()
main = do
    opts <- execParser parseOptionsInfo
    state <- do
        pure
            AppState
                { verbosity = fromMaybe LevelWarn opts.verbosity
                }
    flip runLoggingT (printLog state.verbosity) $
        runApp state $
            case opts.command of
                Update o -> update o
                Install o -> install =<< validateParsedInstallOpts o
