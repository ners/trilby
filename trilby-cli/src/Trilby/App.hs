module Trilby.App where

import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger.CallStack (LogLevel, LoggingT, MonadLogger, MonadLoggerIO, logInfo)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.List (intercalate)
import Data.List.Extra (split)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Environment (getEnv, setEnv)
import Trilby.Version qualified as Trilby
import UnliftIO (MonadIO (liftIO), MonadUnliftIO)
import "base" Prelude

data AppState = AppState
    { verbosity :: LogLevel
    , hostname :: Text
    }
    deriving stock (Generic)

newtype AppT m a = App
    { _runApp :: ReaderT AppState (LoggingT m) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadReader AppState
        , MonadLogger
        , MonadLoggerIO
        , MonadIO
        , MonadUnliftIO
        )

type App = AppT IO

runApp :: (MonadIO m) => AppState -> AppT m a -> LoggingT m a
runApp state App{..} = flip runReaderT state do
    liftIO do
        let nixBinPath = "/run/current-system/sw/bin" :: FilePath
        oldPath <- getEnv "PATH"
        unless (nixBinPath `elem` split (== ':') oldPath) do
            let newPath = intercalate ":" [nixBinPath, oldPath]
            setEnv "PATH" newPath
    logInfo Trilby.fullVersionString
    _runApp
