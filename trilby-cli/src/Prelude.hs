{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Prelude
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.Extra
    , module Control.Monad.Logger.CallStack
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans
    , module Data.Aeson
    , module Data.Bool
    , module Data.ByteString
    , module Data.ByteString.Lazy
    , module Data.Char
    , module Data.Default
    , module Data.Foldable
    , module Data.Functor
    , module Data.List.Extra
    , module Data.List.NonEmpty
    , module Data.Maybe
    , module Data.Semigroup
    , module Data.String
    , module Data.Text
    , module Data.Traversable
    , module Data.Tuple.Extra
    , module Debug.Trace
    , module GHC.Generics
    , module GHC.IsList
    , module GHC.Stack
    , module Nix.Expr.Types
    , module Nix.Pretty
    , module Nix.TH
    , module Path
    , module Path.IO
    , module Prelude
    , module System.Exit
    , module System.IO
    , module Text.Read
    , module Trilby.App
    , module Trilby.FlakeRef
    , module UnliftIO
    , module UnliftIO.Directory
    , module UnliftIO.Environment
    )
where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Logger.CallStack (LogLevel (..), logDebug, logError, logInfo, logWarn)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State (MonadState, evalStateT, execStateT, get, put)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Foldable
import Data.Functor
import Data.Generics.Labels ()
import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List.Extra (headDef, (!?))
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe
import Data.Monoid (First)
import Data.Semigroup (sconcat)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable
import Data.Tuple.Extra (fst3)
import Debug.Trace
import GHC.Generics (Generic)
import GHC.IsList hiding (toList)
import GHC.Stack (HasCallStack)
import Nix.Expr.Types
import Nix.Pretty (prettyNix)
import Nix.TH (ToExpr (toExpr), nix)
import Options.Applicative
    ( Mod
    , OptionFields
    , Parser
    , completeWith
    , eitherReader
    , flag'
    , help
    , long
    , maybeReader
    , option
    , showDefault
    , showDefaultWith
    )
import Path
    ( Abs
    , Dir
    , File
    , Path
    , Rel
    , SomeBase (..)
    , dirname
    , filename
    , mkAbsDir
    , mkAbsFile
    , mkRelDir
    , mkRelFile
    , parent
    , parseAbsDir
    , parseAbsFile
    , parseRelDir
    , parseRelFile
    , parseSomeFile
    , toFilePath
    , (</>)
    )
import Path.IO
    ( AnyPath
    , Permissions (writable)
    , canonicalizePath
    , doesDirExist
    , doesFileExist
    , getPermissions
    , withSystemTempDir
    , withSystemTempFile
    )
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO (BufferMode (NoBuffering), IO)
import System.Posix (getEffectiveUserID)
import System.Process.Typed (StreamSpec)
import System.Process.Typed qualified as Process
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (..), ReadPrec, readMaybe)
import Trilby.App (App, AppState (..))
import Trilby.FlakeRef
import UnliftIO hiding (withSystemTempDirectory, withSystemTempFile, withTempFile)
import UnliftIO.Directory (getCurrentDirectory, setCurrentDirectory)
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.Environment
import "base" Prelude hiding (unzip, writeFile)

rootDir :: Path Abs Dir
rootDir = $(mkAbsDir "/")

trilbyHome :: Path Abs Dir -> Path Abs Dir
trilbyHome root = root </> $(mkRelDir "etc/trilby")

parsePath
    :: (Show e)
    => (String -> Either e (Path b t))
    -> Mod OptionFields (Path b t)
    -> Parser (Path b t)
parsePath p = option . eitherReader $ Bifunctor.first ishow . p

parseYesNo :: String -> String -> Parser Bool
parseYesNo yesLong yesHelp = flag' True (long yesLong <> help yesHelp) <|> flag' False (long noLong)
  where
    noLong = "no-" <> yesLong

parseChoiceWith :: forall a. (a -> String) -> (String -> Maybe a) -> Mod OptionFields a -> [a] -> Parser a
parseChoiceWith show' read' m xs = option (maybeReader read') $ m <> showDefaultWith show' <> completeWith options
  where
    options = show' <$> xs

parseChoice :: forall a. (Show a, Read a) => Mod OptionFields a -> [a] -> Parser a
parseChoice m xs = option (maybeReader readMaybe) $ m <> showDefault <> completeWith options
  where
    options = show @a <$> xs

parseEnum :: (Bounded a, Enum a, Show a, Read a) => Mod OptionFields a -> Parser a
parseEnum = flip parseChoice [minBound .. maxBound]

errorExit :: Text -> App a
errorExit msg = logError msg >> liftIO exitFailure
{-# INLINE errorExit #-}

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

readPrecBoundedEnumOn
    :: forall a
     . (Show a, Bounded a, Enum a)
    => (String -> String)
    -> ReadPrec a
readPrecBoundedEnumOn m = ReadPrec.lift . ReadP.choice $ tryChoose <$> [minBound .. maxBound]
  where
    tryChoose :: a -> ReadP a
    tryChoose a = a <$ ReadP.string (m $ show a)

readPrecBoundedEnum :: (Show a, Bounded a, Enum a) => ReadPrec a
readPrecBoundedEnum = readPrecBoundedEnumOn id

fromText :: (IsString s) => Text -> s
fromText = fromString . Text.unpack

fromPath :: (IsString s) => Path b t -> s
fromPath = fromString . toFilePath

fromSomeBase :: (IsString s) => SomeBase t -> s
fromSomeBase (Abs f) = fromPath f
fromSomeBase (Rel f) = fromPath f

fromListSafe :: a -> [a] -> NonEmpty a
fromListSafe x = fromMaybe (x :| []) . nonEmpty

firstLine :: Text -> Text
firstLine = headDef "" . Text.lines

prepend :: (Semigroup (f a), Applicative f) => a -> f a -> f a
prepend x xs = pure x <> xs

append :: (Semigroup (f a), Applicative f) => a -> f a -> f a
append x xs = xs <> pure x

type Process' a = NonEmpty Text -> App a

type Process a = NonEmpty Text -> StreamSpec 'Process.STInput () -> App a

readProcess :: Process (ExitCode, LazyByteString)
readProcess (x :| xs) input = do
    logInfo $ Text.unwords (x : xs)
    (exitCode, stdout) <-
        Process.readProcessStdout . Process.setStdin input $
            Process.proc (Text.unpack x) (Text.unpack <$> xs)
    pure (exitCode, stdout)

readProcess' :: Process' (ExitCode, LazyByteString)
readProcess' = flip readProcess Process.inherit

readProcessOut :: Process LazyByteString
readProcessOut xs input =
    readProcess xs input >>= \case
        (ExitSuccess, out) -> pure out
        (code, _) -> liftIO $ exitWith code

readProcessOut' :: Process' LazyByteString
readProcessOut' = flip readProcessOut Process.inherit

readProcessOutText :: Process Text
readProcessOutText xs input = Text.decodeUtf8 . LazyByteString.toStrict <$> readProcessOut xs input

readProcessOutText' :: Process' Text
readProcessOutText' = flip readProcessOutText Process.inherit

readProcessOutJson :: (FromJSON a) => Process a
readProcessOutJson xs input = either (errorExit . fromString) pure . Aeson.eitherDecode =<< readProcessOut xs input

readProcessOutJson' :: (FromJSON a) => Process' a
readProcessOutJson' = flip readProcessOutJson Process.inherit

runShell :: Text -> App ()
runShell t = do
    logInfo t
    Process.runProcess (Process.shell (Text.unpack t)) >>= \case
        ExitSuccess -> pure ()
        code -> liftIO $ exitWith code

readShell :: Text -> App (ExitCode, LazyByteString, LazyByteString)
readShell t = do
    logInfo t
    Process.readProcess . Process.shell . Text.unpack $ t

readShellOut :: Text -> App LazyByteString
readShellOut t =
    readShell t >>= \case
        (ExitSuccess, out, _) -> pure out
        (code, _, _) -> liftIO $ exitWith code

readShellOutText :: Text -> App Text
readShellOutText t = Text.decodeUtf8 . LazyByteString.toStrict <$> readShellOut t

runProcess :: Process ()
runProcess (x :| xs) input = do
    logInfo $ Text.unwords (x : xs)
    Process.runProcess_ . Process.setStdin input $
        Process.proc (Text.unpack x) (Text.unpack <$> xs)

runProcess' :: Process' ()
runProcess' = flip runProcess Process.inherit

runProcess_ :: Process ()
runProcess_ (x :| xs) input = do
    logInfo $ Text.unwords (x : xs)
    out <- ifM (verbosityAtLeast LevelInfo) (pure Process.inherit) (pure Process.nullStream)
    Process.runProcess_ . Process.setStdin input . Process.setStdout out $
        Process.proc (Text.unpack x) (Text.unpack <$> xs)

runProcess'_ :: Process' ()
runProcess'_ = flip runProcess_ Process.inherit

isRoot :: App Bool
isRoot = (0 ==) <$> liftIO getEffectiveUserID

asUser :: Process a -> Process a
asUser c = c

asRoot :: Process' a -> Process' a
asRoot c t = ifM isRoot (c t) (c $ prepend "sudo" t)

cached :: Process' (ExitCode, LazyByteString) -> Process' (ExitCode, LazyByteString)
cached c t = do
    var <- Reader.asks commandCache
    HashMap.lookup t <$> readTVarIO var & fromMaybeM do
        o <- c t
        atomically . modifyTVar var . HashMap.insert t $ o
        pure o

singleQuoted :: Text -> Text
singleQuoted t = d <> escape t <> d
  where
    d = "'" :: Text
    escape = Text.replace d (d <> "\\" <> d <> d)

doubleQuoted :: Text -> Text
doubleQuoted t = d <> escape t <> d
  where
    d = "\"" :: Text
    escape = Text.replace d (d <> "\\" <> d <> d)

isAbsolute :: Path b t -> Bool
isAbsolute p = Just '/' == listToMaybe (toFilePath p)

isRelative :: Path b t -> Bool
isRelative = not . isAbsolute

createDir :: Path b Dir -> App ()
createDir dir = do
    logDebug $ "createDir " <> fromPath dir
    parentWritable <- handleAny (const $ pure False) $ writable <$> getPermissions (parent dir)
    (if parentWritable then id else asRoot) runProcess'_ ["mkdir", "-p", fromPath dir]

data Owner = Owner {uid :: Int, gid :: Int}

instance Show Owner where
    show Owner{..} = show uid <> ":" <> show gid

currentOwner :: App Owner
currentOwner = do
    uid <- read . fromText <$> readProcessOutText' ["id", "-u"]
    gid <- read . fromText <$> readProcessOutText' ["id", "-g"]
    pure Owner{..}

changeOwner :: Owner -> Path b t -> App ()
changeOwner owner dir = asRoot runProcess'_ ["chown", "-R", ishow owner, fromPath dir]

ensureDir :: Path b Dir -> App ()
ensureDir dir = do
    logDebug $ "ensureDir " <> fromPath dir
    owner <- currentOwner
    unlessM (doesDirExist dir) do
        createDir dir
        changeOwner owner dir

inDir :: Path b Dir -> App a -> App a
inDir d a = do
    ensureDir d
    d' <- getCurrentDirectory
    setCurrentDirectory $ toFilePath d
    a <- a
    setCurrentDirectory d'
    pure a

inTmpDir :: (Path Abs Dir -> App a) -> App a
inTmpDir a = do
    d <- Reader.asks tmpDir
    inDir d $ a d

writeFile :: Path b File -> Text -> App ()
writeFile f t = do
    logDebug $ "writeFile " <> fromPath f <> "\n" <> t
    ensureDir $ parent f
    liftIO $ Text.writeFile (toFilePath f) t

withTempFile :: Path Rel File -> (Path Abs File -> App a) -> App a
withTempFile file action = do
    tmpDir <- Reader.asks tmpDir
    let tmpFile = tmpDir </> file
    withFile (toFilePath tmpFile) ReadWriteMode \handle -> do
        hClose handle
        action tmpFile

getSymlinkTarget :: (MonadIO m) => (FilePath -> m (Path b2 t2)) -> SomeBase File -> m (Path b2 t2)
getSymlinkTarget = (UnliftIO.canonicalizePath . fromSomeBase >=>)

is :: a -> Getting (First c) a c -> Bool
is a c = isJust $ a ^? c

verbosityAtLeast :: LogLevel -> App Bool
verbosityAtLeast v = Reader.asks $ (v >=) . verbosity

withTrace :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
withTrace f (x :| xs) =
    verbosityAtLeast LevelDebug
        >>= f
            . (x :|)
            . bool xs ("--show-trace" : xs)

containsAnyOf :: (Each s s a a, Foldable t, Eq a) => t a -> s -> Bool
containsAnyOf = anyOf each . flip elem

containsNoneOf :: (Each s s a a, Foldable t, Eq a) => t a -> s -> Bool
containsNoneOf = noneOf each . flip elem

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

genM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t (a, b))
genM f = mapM \a -> (a,) <$> f a

which :: FilePath -> App [Path Abs File]
which f = do
    f <- parseRelFile f
    paths <- maybe (pure mempty) (mapM parseAbsDir . List.split (':' ==)) =<< lookupEnv "PATH"
    filterM doesFileExist $ paths <&> (</> f)

replaceSuffix :: Text -> Text -> Text -> Text
replaceSuffix oldSuffix newSuffix text = maybe text (<> newSuffix) $ Text.stripSuffix oldSuffix text
