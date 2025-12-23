module Effectful.Path where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (filterM, (=<<))
import Control.Monad.Extra (findM)
import Data.Eq ((==))
import Data.Function (flip, ($), (.))
import Data.Functor ((<$>))
import Data.List.Extra qualified as List
import Data.Maybe (Maybe, maybe)
import Effectful (Eff, (:>))
import Effectful.Environment (Environment, lookupEnv, setEnv)
import Effectful.FileSystem (FileSystem, doesFileExist)
import System.FilePath ((</>))
import System.FilePath.Posix (FilePath)

getPath :: (Environment :> es) => Eff es [FilePath]
getPath = maybe [] (List.split (== ':')) <$> lookupEnv "PATH"

overPath :: (Environment :> es) => ([FilePath] -> [FilePath]) -> Eff es ()
overPath f = setEnv "PATH" . List.intercalate ":" . f =<< getPath

findBinaries :: (Environment :> es, FileSystem :> es) => [FilePath] -> Eff es [FilePath]
findBinaries binaries = do
    paths <- getPath
    filterM doesFileExist $ flip (</>) <$> binaries <*> paths

findFirstBinary :: (Environment :> es, FileSystem :> es) => [FilePath] -> Eff es (Maybe FilePath)
findFirstBinary binaries = do
    paths <- getPath
    findM doesFileExist $ flip (</>) <$> binaries <*> paths

findBinary :: (Environment :> es, FileSystem :> es) => FilePath -> Eff es (Maybe FilePath)
findBinary = findFirstBinary . pure
