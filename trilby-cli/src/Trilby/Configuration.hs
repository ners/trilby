module Trilby.Configuration where

import Trilby.Host
import Trilby.Prelude

data Configuration = Configuration
    { name :: Text
    , host :: Host
    }
    deriving stock (Generic, Eq, Ord, Show)

fromHost
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Host
    -> Eff es Configuration
fromHost host' = do
    host <- canonicalHost host'
    name <- hostname host
    pure Configuration{..}
