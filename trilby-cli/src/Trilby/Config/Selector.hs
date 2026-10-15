module Trilby.Config.Selector where

import Data.Text qualified as Text
import System.FilePath.Glob (Pattern)
import System.FilePath.Glob qualified as Glob
import Trilby.Prelude

newtype Selector = Selector [Pattern]
    deriving stock (Eq, Show)

instance IsString Selector where
    fromString = Selector . fmap (Glob.compile . Text.unpack) . Text.splitOn "," . fromString

select :: Selector -> [Text] -> [Text]
select (Selector patterns) = filter \name -> any (`Glob.match` Text.unpack name) patterns
