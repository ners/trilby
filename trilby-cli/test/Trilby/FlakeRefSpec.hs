module Trilby.FlakeRefSpec (spec) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.FlakeRef
import Prelude

instance Arbitrary FlakeRef where
    arbitrary = do
        url <- genPart (`notElem` ("#" :: String))
        output <- listOf $ genPart (`notElem` ("#." :: String))
        pure FlakeRef{..}
      where
        genPart :: (Char -> Bool) -> Gen Text
        genPart p = Text.pack <$> listOf1 (arbitrary `suchThat` (\c -> p c && not (isSpace c)))

spec :: Spec
spec = prop "FlakeRef show/read round trip" \(ref :: FlakeRef) -> read (show ref) `shouldBe` ref
