module Effectful.Path where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (filterM)
import Control.Monad.Extra (findM)
import Data.Eq ((==))
import Data.Function (flip, ($), (.))
import Data.Functor ((<$>))
import Data.List.Extra qualified as List
import Data.Maybe (Maybe, maybe)
import Effectful (Eff, (:>))
import Effectful.Environment (Environment, lookupEnv)
import Effectful.FileSystem (FileSystem, doesFileExist)
import System.FilePath ((</>))
import System.FilePath.Posix (FilePath)

getSearchPath :: (Environment :> es) => Eff es [FilePath]
getSearchPath = maybe [] (List.split (== ':')) <$> lookupEnv "PATH"

findBinaries :: (Environment :> es, FileSystem :> es) => [FilePath] -> Eff es [FilePath]
findBinaries binaries = do
    paths <- getSearchPath
    filterM doesFileExist $ flip (</>) <$> binaries <*> paths

findFirstBinary :: (Environment :> es, FileSystem :> es) => [FilePath] -> Eff es (Maybe FilePath)
findFirstBinary binaries = do
    paths <- getSearchPath
    findM doesFileExist $ flip (</>) <$> binaries <*> paths

findBinary :: (Environment :> es, FileSystem :> es) => FilePath -> Eff es (Maybe FilePath)
findBinary = findFirstBinary . pure
