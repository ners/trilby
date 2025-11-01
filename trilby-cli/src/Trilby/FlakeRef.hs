module Trilby.FlakeRef where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Prelude

data FlakeRef = FlakeRef
    { url :: Text
    , output :: [Text]
    }
    deriving stock (Generic)

instance IsString FlakeRef where
    fromString (fromString -> Text.break (== '#') -> (url, Text.split (== '.') -> output)) =
        FlakeRef{..}

instance Show FlakeRef where
    show FlakeRef{output = [], ..} = Text.unpack url
    show FlakeRef{..} = Text.unpack $ url <> "#" <> Text.intercalate "." output
