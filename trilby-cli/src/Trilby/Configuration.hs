module Trilby.Configuration where

import Trilby.Host
import Prelude

data Configuration = Configuration {name :: Text, host :: Host}
    deriving stock (Generic, Eq, Ord)

fromHost :: Host -> App Configuration
fromHost host' = do
    host <- canonicalHost host'
    name <- hostname host
    pure Configuration{..}
