module Trilby.SystemSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.System
import Prelude

instance Arbitrary Architecture where
    arbitrary = elements ["x86_64", "aarch64"]

instance Arbitrary Kernel where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary System where
    arbitrary = do
        architecture <- arbitrary
        kernel <- arbitrary
        pure System{..}

spec :: Spec
spec = do
    prop "Architecture show/read round-trip" \(architecture :: Architecture) ->
        read (show architecture) `shouldBe` architecture
    prop "Kernel show/read round-trip" \(kernel :: Kernel) -> read (show kernel) `shouldBe` kernel
    prop "System show/read round-trip" \(system :: System) -> read (show system) `shouldBe` system
