module Trilby.Config.EditSpec (spec) where

import Data.Either (isRight)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Trilby.Config.Edit (Element (..), Location (..))
import Trilby.Config.Edit qualified as Edit
import Prelude

location :: [Text] -> Location
location condition = Location{attribute = ["environment", "systemPackages"], condition}

sources :: [Text]
sources =
    [ "{ pkgs, lib, trilby, ... }:\n{\n  environment.systemPackages = with pkgs; [\n    git\n    jq\n  ];\n}\n"
    , "{ pkgs, lib, trilby, ... }:\n{\n  environment.systemPackages = with pkgs; [ git jq ];\n}\n"
    , "{ pkgs, lib, trilby, ... }:\n{\n  environment.systemPackages = with pkgs; [ git ]\n    ++ lib.optionals (trilby.edition == \"workstation\") [ firefox ];\n}\n"
    , "{ pkgs, lib, trilby, ... }:\n{\n  environment = {\n    systemPackages = [ pkgs.git ];\n  };\n}\n"
    ]

spec :: Spec
spec =
    prop "removing an added package restores the file, and the result parses" $
        forAll (Text.pack . ('x' :) <$> listOf (elements ['a' .. 'z'])) $
            \item ->
                forAll (elements sources) \source ->
                    forAll (elements [[], ["trilby.edition == \"server\""]]) \condition ->
                        let added = Edit.addElement (location condition) (Package item) source
                         in conjoin
                                [ property $ isRight (added >>= Edit.parse)
                                , (added >>= Edit.removeElement (location condition).attribute (Edit.sameElement $ Package item))
                                    === Right source
                                ]
