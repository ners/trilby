{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE TypeApplications #-}

module Trilby.App where

import Control.Applicative
import Control.Monad.Extra
import Data.Aeson (FromJSON, ToJSON, Value, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.HashMap.Internal.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty, (<|))
import Data.Text (Text)
import Effectful
import Effectful.Concurrent.STM (Concurrent, TVar, atomically, modifyTVar, readTVarIO)
import Effectful.Error.Static (HasCallStack)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import GHC.Generics (Generic)
import Path (Abs, Dir, Path)
import Trilby.Host (Host)
import Trilby.Log
import Trilby.Util (ishow)
import Prelude

data AppState = AppState
    { verbosity :: LogLevel
    , commandCache :: TVar (HashMap (NonEmpty Text) Value)
    , tmpDir :: Path Abs Dir
    }
    deriving stock (Generic)

cached
    :: (HasCallStack, Concurrent :> es, Reader AppState :> es, Reader Host :> es, ToJSON a, FromJSON a)
    => (NonEmpty Text -> Eff es a)
    -> NonEmpty Text
    -> Eff es a
cached c t = do
    var <- Reader.asks commandCache
    host <- Reader.ask @Host
    let key = ishow host <| t
    hit <- (Aeson.parseMaybe Aeson.parseJSON =<<) . HashMap.lookup key <$> readTVarIO var
    pure hit & fromMaybeM do
        a <- c t
        atomically . modifyTVar var . HashMap.insert key $ toJSON a
        pure a
