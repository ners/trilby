module Trilby.FlakeRef where

import Data.Char (isSpace)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (..))
import Prelude

data FlakeRef = FlakeRef
    { url :: Text
    , output :: [Text]
    }
    deriving stock (Generic, Eq)

instance IsString FlakeRef where
    fromString (fromString -> Text.break (== '#') -> (url, Text.drop 1 -> fragment)) =
        FlakeRef{url, output = if Text.null fragment then [] else Text.split (== '.') fragment}

instance Read FlakeRef where
    readPrec = fromString <$> ReadPrec.lift (ReadP.munch1 $ not . isSpace)

instance Show FlakeRef where
    show FlakeRef{output = [], ..} = Text.unpack url
    show FlakeRef{..} = Text.unpack $ url <> "#" <> Text.intercalate "." output
