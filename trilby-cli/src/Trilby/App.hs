{-# LANGUAGE FieldSelectors #-}

module Trilby.App where

import Control.Applicative
import Control.Monad.Extra
import Data.Aeson (FromJSON, ToJSON, Value, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Generics.Labels ()
import Data.HashMap.Internal.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Effectful
import Effectful.Concurrent.STM (Concurrent, TVar, atomically, modifyTVar, readTVarIO)
import Effectful.Error.Static (HasCallStack)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Trilby.Log
import Prelude

data AppState = AppState
    { verbosity :: LogLevel
    , commandCache :: TVar (HashMap (NonEmpty Text) Value)
    , tmpDir :: Path Abs Dir
    }
    deriving stock (Generic)

cached
    :: (HasCallStack, Concurrent :> es, Reader AppState :> es, ToJSON a, FromJSON a)
    => (NonEmpty Text -> Eff es a)
    -> NonEmpty Text
    -> Eff es a
cached c t = do
    var <- Reader.asks commandCache
    value <- flip fromMaybeM (HashMap.lookup t <$> readTVarIO var) do
        o <- toJSON <$> c t
        atomically . modifyTVar var . HashMap.insert t $ o
        pure o
    either error pure $ Aeson.parseEither Aeson.parseJSON value
