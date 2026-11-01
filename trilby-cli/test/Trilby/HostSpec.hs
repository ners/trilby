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
    describe "sshTarget" do
        it "reads hostname and port" do
            sshTarget "user dragoncat\nhostname 10.0.1.32\nport 22\nproxyusefdpass no\n"
                `shouldBe` Just ("10.0.1.32", "22")
        it "defaults the port to 22" do
            sshTarget "hostname orville.nest\n" `shouldBe` Just ("orville.nest", "22")
        it "rejects proxied connections" do
            sshTarget "hostname 10.0.1.14\nport 22\nproxyjump bastion\n" `shouldBe` Nothing
            sshTarget "hostname 10.0.1.14\nproxycommand nc %h %p\n" `shouldBe` Nothing
        it "accepts proxies set to none" do
            sshTarget "hostname 10.0.1.14\nport 2222\nproxyjump none\n"
                `shouldBe` Just ("10.0.1.14", "2222")
