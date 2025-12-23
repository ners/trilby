module Trilby.System where

import Data.Char (isDigit, isLetter)
import Data.Text qualified as Text
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Trilby.Prelude

newtype Architecture = Architecture Text
    deriving newtype (Eq, Ord, IsString)

instance Show Architecture where
    show (Architecture a) = Text.unpack a

instance Read Architecture where
    readPrec = fromString <$> ReadPrec.lift (ReadP.munch allowed)
      where
        allowed :: Char -> Bool
        allowed c = or @[] [isLetter c, isDigit c, c == '_']

instance ToExpr Architecture where
    toExpr = toExpr . show

data Kernel
    = Linux
    | Darwin
    deriving stock (Generic, Eq, Ord, Show, Bounded, Enum)

instance Read Kernel where
    readPrec = readPrecBoundedEnumOn (fmap toLower)

data System = System
    { architecture :: Architecture
    , kernel :: Kernel
    }
    deriving stock (Generic, Eq, Ord)

instance Show System where
    show System{..} = show architecture <> "-" <> fmap toLower (show kernel)

instance Read System where
    readPrec = do
        architecture <- fromString <$> ReadPrec.lift (ReadP.munch (/= '-'))
        ReadPrec.lift $ ReadP.char '-'
        kernel <- readPrec
        pure System{..}

instance ToExpr System where
    toExpr = toExpr . show
