{-# LANGUAGE FieldSelectors #-}

module Trilby.App where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger.CallStack (LogLevel, LoggingT, MonadLogger, MonadLoggerIO, logInfo)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import System.Exit (ExitCode)
import Trilby.Version qualified as Trilby
import UnliftIO (MonadIO, MonadUnliftIO, TVar)
import "base" Prelude

data AppState = AppState
    { verbosity :: LogLevel
    , commandCache :: TVar (HashMap (NonEmpty Text) (ExitCode, Text))
    , tmpDir :: Path Abs Dir
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
    logInfo Trilby.fullVersionString
    _runApp
