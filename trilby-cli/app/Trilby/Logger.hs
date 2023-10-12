{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}

module Trilby.Logger where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Logger (LoggingT, MonadLogger, MonadLoggerIO)
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Control (MonadBaseControl)
import UnliftIO (MonadIO, MonadUnliftIO)
import Prelude

newtype ErrorLoggingT m a = ErrorLoggingT {runErrorLoggingT :: LoggingT m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadUnliftIO
        , MonadLogger
        , MonadLoggerIO
        , MonadTrans
        , MonadThrow
        , MonadCatch
        , MonadMask
        )

deriving newtype instance (MonadBase b m) => MonadBase b (ErrorLoggingT m)

deriving newtype instance (MonadBaseControl b m) => MonadBaseControl b (ErrorLoggingT m)
