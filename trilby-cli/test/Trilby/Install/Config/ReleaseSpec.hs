module Trilby.Install.Config.ReleaseSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.Install.Config.Release
import Prelude

instance Arbitrary Release where
    arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
    prop "Release show/read round trip" \(release :: Release) -> read (show release) `shouldBe` release
