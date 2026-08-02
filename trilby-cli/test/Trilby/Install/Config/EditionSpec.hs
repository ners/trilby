module Trilby.Install.Config.EditionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.Install.Config.Edition
import Prelude

instance Arbitrary Edition where
    arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
    prop "Edition show/read round-trip" \(edition :: Edition) -> read (show edition) `shouldBe` edition
