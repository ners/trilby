module Trilby.Disko.FilesystemSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.Disko.Filesystem
import Prelude

instance Arbitrary Format where
    arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
    prop "Format show/read round-trip" \(format :: Format) -> read (show format) `shouldBe` format
