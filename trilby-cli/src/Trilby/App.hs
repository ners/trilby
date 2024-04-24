module Trilby.App where

import "base" Prelude
import Control.Monad.Logger (LogLevel, LoggingT, MonadLogger, MonadLoggerIO, logInfo)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Text (Text)
import Data.List (intercalate)
import Data.List.Extra (split)
import GHC.Generics (Generic)
import UnliftIO (MonadIO(liftIO), MonadUnliftIO)
import Trilby.Version qualified as Trilby
import System.Environment (setEnv, getEnv)
import Control.Monad (unless)

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
        , MonadReader AppState
        , MonadLogger
        , MonadLoggerIO
        , MonadIO
        , MonadUnliftIO
        )

type App = AppT IO

runApp :: MonadIO m => AppState -> AppT m a -> LoggingT m a
runApp state App{..} = flip runReaderT state do
    liftIO do
        let nixBinPath = "/run/current-system/sw/bin" :: FilePath
        oldPath <- getEnv "PATH"
        unless (nixBinPath `elem` split (== ':') oldPath) do
            let newPath = intercalate ":" [nixBinPath, oldPath]
            setEnv "PATH" newPath
    $(logInfo) Trilby.fullVersionString
    _runApp
