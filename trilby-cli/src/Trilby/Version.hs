module Trilby.Version where

import Data.List qualified as List
import Data.String (IsString (fromString))
import Data.Version (Version, showVersion)
import Paths_trilby_cli qualified
import Prelude

name :: (IsString s) => s
name = "trilby-cli"

version :: Version
version = Paths_trilby_cli.version

fullVersionString :: (IsString s) => s
fullVersionString = fromString . List.unwords $ [name, showVersion version]
