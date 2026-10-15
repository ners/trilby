module Trilby.Config.CacheSpec (spec) where

import Data.Aeson (Value (String))
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.Config.Cache qualified as Cache
import Prelude

spec :: Spec
spec = do
    prop "an entry is found again with its key" \key value ->
        Cache.decode (Text.pack key) (Cache.encode (Text.pack key) (String $ Text.pack value))
            === Just (String $ Text.pack value)
    prop "an entry is missed with another key" \key value ->
        Cache.decode (Text.pack key <> "x") (Cache.encode (Text.pack key) (String $ Text.pack value))
            === Nothing
