module Trilby.App where

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Control.Monad.Logger (LogLevel, LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Data.Functor (Functor)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO (IO)
import UnliftIO (MonadIO, MonadUnliftIO)

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

runApp :: AppState -> AppT m a -> LoggingT m a
runApp state App{..} = runReaderT _runApp state
