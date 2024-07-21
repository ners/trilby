module Trilby.Version where

import Data.FileEmbed
import Data.List qualified as List
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import "base" Prelude

cabalFile :: (IsString s) => s
cabalFile = $(embedStringFile "trilby-cli.cabal")

cabalField :: (IsString s) => Text -> s
cabalField ((<> ":") -> field) =
    fromString
        . Text.unpack
        . List.head
        . List.tail
        . List.dropWhile (/= field)
        . Text.words
        $ cabalFile

name :: (IsString s) => s
name = cabalField "name"

version :: (IsString s) => s
version = cabalField "version"

fullVersionString :: (IsString s) => s
fullVersionString = fromString $ unwords [name, version]
