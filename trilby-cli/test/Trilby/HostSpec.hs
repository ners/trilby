module Trilby.HostSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.Host
import Prelude

instance Arbitrary Host where
    arbitrary =
        oneof
            [ pure Localhost
            , do
                username <- oneof [pure Nothing, Just <$> genPart]
                hostname <- genPart
                pure Host{..}
            ]
      where
        genPart :: Gen Text
        genPart = Text.pack <$> listOf1 (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> "-.")

spec :: Spec
spec = do
    prop "Host show/read round trip" \(host :: Host) -> read (show host) `shouldBe` host
