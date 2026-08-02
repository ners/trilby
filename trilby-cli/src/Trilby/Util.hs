module Trilby.Util where

import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Path
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (ReadPrec)
import Prelude

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

readPrecBoundedEnumOn
    :: forall a
     . (Show a, Bounded a, Enum a)
    => ( String
         -> String
       )
    -> ReadPrec a
readPrecBoundedEnumOn m = ReadPrec.lift . ReadP.choice $ tryChoose <$> [minBound .. maxBound]
  where
    -- Accepts both the transformed wire form (e.g. lowercase) and 'show's own
    -- output verbatim, so `read . show` round-trips regardless of the mapping.
    -- (De-duplicated: trying the same literal twice makes ReadP report the
    -- otherwise-unambiguous parse as ambiguous.)
    tryChoose :: a -> ReadP a
    tryChoose a = a <$ ReadP.choice (ReadP.string <$> List.nub [m $ show a, show a])

readPrecBoundedEnum :: (Show a, Bounded a, Enum a) => ReadPrec a
readPrecBoundedEnum = readPrecBoundedEnumOn id

fromText :: (IsString s) => Text -> s
fromText = fromString . Text.unpack

fromPath :: (IsString s) => Path b t -> s
fromPath = fromString . toFilePath

fromSomeBase :: (IsString s) => SomeBase t -> s
fromSomeBase (Abs f) = fromPath f
fromSomeBase (Rel f) = fromPath f

fromListSafe :: a -> [a] -> NonEmpty a
fromListSafe x = fromMaybe (x :| []) . nonEmpty

firstLine :: Text -> Text
firstLine = List.headDef "" . Text.lines

prepend :: (Semigroup (f a), Applicative f) => a -> f a -> f a
prepend x xs = pure x <> xs

append :: (Semigroup (f a), Applicative f) => a -> f a -> f a
append x xs = xs <> pure x

singleQuoted :: Text -> Text
singleQuoted t = d <> escape t <> d
  where
    d = "'" :: Text
    escape = Text.replace d (d <> "\\" <> d <> d)

doubleQuoted :: Text -> Text
doubleQuoted t = d <> escape t <> d
  where
    d = "\"" :: Text
    escape = Text.replace d (d <> "\\" <> d <> d)

isAbsolute :: Path b t -> Bool
isAbsolute p = Just '/' == listToMaybe (toFilePath p)

isRelative :: Path b t -> Bool
isRelative = not . isAbsolute
