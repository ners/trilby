{-# LANGUAGE FieldSelectors #-}

module Trilby.App where

import Data.Aeson (Value)
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Monoid (mempty))
import Data.Text (Text)
import Effectful (Eff, IOE, MonadIO, inject, liftIO, runEff)
import Effectful.Concurrent.STM (Concurrent, TVar, newTVarIO, runConcurrent)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem.Path.IO (FileSystem, runFileSystem)
import Effectful.Process.Typed (TypedProcess, runTypedProcess)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Temporary.Path.IO (Temporary, runTemporary, withSystemTempDir)
import Effectful.Time (Time, runTime)
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Trilby.Log
import Trilby.Version qualified as Trilby

data AppState = AppState
    { verbosity :: LogLevel
    , commandCache :: TVar (HashMap (NonEmpty Text) Value)
    , tmpDir :: Path Abs Dir
    }
    deriving stock (Generic)

type App = Eff '[IOE, Fail, Environment, Reader AppState, Concurrent, TypedProcess, Time, Temporary, FileSystem, Log]

runApp :: (MonadIO m) => LogLevel -> App a -> m a
runApp verbosity action = do
    liftIO
        . runEff
        . runLog verbosity
        . runFileSystem
        . runTemporary
        . runTime
        . runTypedProcess
        . runConcurrent
        . runEnvironment
        . runFailIO
        $ do
            logInfo_ Trilby.fullVersionString
            commandCache <- newTVarIO mempty
            withSystemTempDir "trilby" \tmpDir ->
                runReader AppState{..} . inject $ action
