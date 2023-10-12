module Trilby.App where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger (LogLevel, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Functor (Functor)
import GHC.Generics (Generic)
import System.IO (IO)
import Trilby.Logger (ErrorLoggingT)
import UnliftIO (MonadIO, MonadUnliftIO, TVar)

newtype AppState = AppState
    { verbosity :: TVar LogLevel
    }
    deriving stock (Generic)

newtype AppT m a = App
    { _runApp :: ReaderT AppState (ErrorLoggingT m) a
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
        , MonadThrow
        , MonadCatch
        , MonadMask
        )

type App = AppT IO

runApp :: AppState -> AppT m a -> ErrorLoggingT m a
runApp state App{..} = runReaderT _runApp state
