{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Trilby.Prelude
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.Extra
    , module Data.Bool
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
    , module Debug.Trace
    , module Effectful
    , module Effectful.Concurrent.STM
    , module Effectful.Environment
    , module Effectful.Exception
    , module Effectful.FileSystem.Path.IO
    , module Effectful.Reader.Static
    , module Effectful.Temporary.Path.IO
    , module GHC.Generics
    , module GHC.IsList
    , module GHC.Stack
    , module Nix.Expr.Types
    , module Nix.Pretty
    , module Nix.TH
    , module Path
    , module Prelude
    , module System.Exit
    , module Text.Read
    , module Trilby.App
    , module Trilby.FlakeRef
    , module Trilby.Log
    , module Trilby.Prelude
    )
where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Extra
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.Bool
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Foldable
import Data.Functor
import Data.Generics.Labels ()
import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List.Extra (headDef, (!?))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe
import Data.Monoid (First)
import Data.Semigroup (sconcat)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable
import Debug.Trace
import Effectful
import Effectful.Concurrent.STM (atomically, modifyTVar, readTVarIO)
import Effectful.Environment (getEnv, lookupEnv, setEnv)
import Effectful.Exception (SomeException (SomeException), handle)
import Effectful.FileSystem.Path.IO (AnyPath, Permissions (writable), XdgDirectory (XdgConfig), canonicalizePath, doesDirExist, getPermissions, getXdgDir)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Temporary.Path.IO (withSystemTempDir, withSystemTempFile)
import GHC.Generics (Generic)
import GHC.IsList hiding (toList)
import GHC.Stack (HasCallStack, withFrozenCallStack)
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
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix (changeWorkingDirectory, getEffectiveUserID, getWorkingDirectory, readSymbolicLink)
import System.Process.Typed qualified as Process
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (..), ReadPrec, readMaybe)
import Trilby.App (App, AppState (..))
import Trilby.FlakeRef
import Trilby.Log
import Trilby.Process
import Prelude hiding (unzip, writeFile)

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

errorExit :: (HasCallStack) => Text -> App a
errorExit msg = logAttention_ msg >> liftIO exitFailure
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

isRoot :: Bool
isRoot = unsafePerformIO getEffectiveUserID == 0

asUser :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asUser = id

asRoot :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asRoot c t = if isRoot then c t else c $ prepend "sudo" t

asRootIf :: Bool -> (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asRootIf b = if b then asRoot else id

asRootWhen :: App Bool -> (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asRootWhen b c t = b >>= \b -> asRootIf b c t

asRootUnless :: App Bool -> (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asRootUnless b = asRootWhen (not <$> b)

cached :: (HasCallStack, ToJSON a, FromJSON a) => (NonEmpty Text -> App a) -> NonEmpty Text -> App a
cached c t = do
    var <- Reader.asks commandCache
    value <- flip fromMaybeM (HashMap.lookup t <$> readTVarIO var) do
        o <- toJSON <$> c t
        atomically . modifyTVar var . HashMap.insert t $ o
        pure o
    either error pure $ Aeson.parseEither Aeson.parseJSON value

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

process_ :: (HasCallStack) => ProcessConfig stdin stdout stderr -> App ()
process_ p = withFrozenCallStack do
    isTrace <- verbosityAtLeast LogInfo
    let output = if isTrace then Process.inherit else Process.nullStream
    runProcess_ . setStdin Process.nullStream . setStdout output . setStderr output $ p

processCode :: (HasCallStack) => ProcessConfig stdin stdout stderr -> App ExitCode
processCode p = withFrozenCallStack do
    isTrace <- verbosityAtLeast LogInfo
    let output = if isTrace then Process.inherit else Process.nullStream
    runProcess . setStdin Process.nullStream . setStdout output . setStderr output $ p

processOutText :: (HasCallStack) => ProcessConfig stdin stdoutIgnored stderr -> App Text
processOutText p = withFrozenCallStack do
    isTrace <- verbosityAtLeast LogInfo
    let stderr = if isTrace then Process.inherit else Process.nullStream
    Text.decodeUtf8 . LazyByteString.toStrict <$> (readProcessStdout_ . setStdin Process.nullStream . setStderr stderr) p

processOutTextLines :: (HasCallStack) => ProcessConfig stdin stdoutIgnored stderr -> App [Text]
processOutTextLines = fmap (fmap Text.strip . Text.lines) . withFrozenCallStack processOutText

processOutTextFirstLine :: (HasCallStack) => ProcessConfig stdin stdoutIgnored stderr -> App (Maybe Text)
processOutTextFirstLine = fmap listToMaybe . withFrozenCallStack processOutTextLines

processCodeOutText :: (HasCallStack) => ProcessConfig stdin stdout stderr -> App (ExitCode, Text)
processCodeOutText p = withFrozenCallStack do
    isTrace <- verbosityAtLeast LogInfo
    let stderr = if isTrace then Process.inherit else Process.nullStream
    second (Text.decodeUtf8 . LazyByteString.toStrict) <$> (readProcessStdout . setStdin Process.nullStream . setStderr stderr) p

processOutJson :: (HasCallStack, FromJSON a) => ProcessConfig stdin stdoutIgnored stderr -> App a
processOutJson p = withFrozenCallStack do
    isTrace <- verbosityAtLeast LogInfo
    let stderr = if isTrace then setStderr Process.inherit else setStderr Process.nullStream
    either fail pure . Aeson.eitherDecode =<< (readProcessStdout_ . setStdin Process.nullStream . stderr) p

shell_ :: (HasCallStack) => NonEmpty Text -> App ()
shell_ = withFrozenCallStack process_ . shell . Text.unwords . NonEmpty.toList

shellOutText :: (HasCallStack) => NonEmpty Text -> App Text
shellOutText = withFrozenCallStack processOutText . shell . Text.unwords . NonEmpty.toList

shellOutTextFirstLine :: (HasCallStack) => NonEmpty Text -> App (Maybe Text)
shellOutTextFirstLine = withFrozenCallStack processOutTextFirstLine . shell . Text.unwords . NonEmpty.toList

shellOutTextLines :: (HasCallStack) => NonEmpty Text -> App [Text]
shellOutTextLines = withFrozenCallStack processOutTextLines . shell . Text.unwords . NonEmpty.toList

cmd_ :: (HasCallStack) => NonEmpty Text -> App ()
cmd_ = withFrozenCallStack process_ . proc

cmdCode :: (HasCallStack) => NonEmpty Text -> App ExitCode
cmdCode = withFrozenCallStack processCode . proc

cmdOutText :: (HasCallStack) => NonEmpty Text -> App Text
cmdOutText = withFrozenCallStack processOutText . proc

cmdCodeOutText :: (HasCallStack) => NonEmpty Text -> App (ExitCode, Text)
cmdCodeOutText = withFrozenCallStack processCodeOutText . proc

cmdOutTextLines :: (HasCallStack) => NonEmpty Text -> App [Text]
cmdOutTextLines = withFrozenCallStack processOutTextLines . proc

cmdOutTextFirstLine :: (HasCallStack) => NonEmpty Text -> App (Maybe Text)
cmdOutTextFirstLine = withFrozenCallStack processOutTextFirstLine . proc

cmdOutJson :: (HasCallStack, FromJSON a) => NonEmpty Text -> App a
cmdOutJson = processOutJson . proc

createDir :: Path b Dir -> App ()
createDir dir = do
    logTrace_ $ "createDir " <> fromPath dir
    asRootUnless parentWritable cmd_ ["mkdir", "-p", fromPath dir]
  where
    parentWritable = handle (const @_ @SomeException $ pure False) $ writable <$> getPermissions (parent dir)

data Owner = Owner {uid :: Int, gid :: Int}

instance Show Owner where
    show Owner{..} = show uid <> ":" <> show gid

currentOwner :: App Owner
currentOwner = do
    uid <- cmdOutJson ["id", "-u"]
    gid <- cmdOutJson ["id", "-g"]
    pure Owner{..}

changeOwner :: Owner -> Path b t -> App ()
changeOwner owner dir = asRoot cmd_ ["chown", "-R", ishow owner, fromPath dir]

ensureDir :: Path b Dir -> App ()
ensureDir dir = do
    logTrace_ $ "ensureDir " <> fromPath dir
    owner <- currentOwner
    unlessM (doesDirExist dir) do
        createDir dir
        changeOwner owner dir

inDir :: Path b Dir -> App a -> App a
inDir d a = do
    d' <- liftIO $ parseAbsDir =<< getWorkingDirectory
    ensureDir d
    liftIO $ changeWorkingDirectory (fromPath d)
    a <- a
    liftIO $ changeWorkingDirectory (fromPath d')
    pure a

writeFile :: (HasCallStack) => Path b File -> Text -> App ()
writeFile f t = do
    logTrace ("writeFile " <> fromPath f) t
    ensureDir $ parent f
    liftIO $ Text.writeFile (fromPath f) t

withTempFile :: Path Rel File -> (Path Abs File -> App a) -> App a
withTempFile file action = do
    tmpDir <- Reader.asks tmpDir
    action $ tmpDir </> file

getSymlinkTarget :: (MonadIO m) => (FilePath -> m (Path b2 t2)) -> SomeBase File -> m (Path b2 t2)
getSymlinkTarget = (liftIO . readSymbolicLink . fromSomeBase >=>)

is :: a -> Getting (First c) a c -> Bool
is a c = isJust $ a ^? c

verbosityAtLeast :: LogLevel -> App Bool
verbosityAtLeast v = Reader.asks $ (v <) . verbosity

withTrace :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
withTrace f (x :| xs) =
    verbosityAtLeast LogTrace
        >>= f
            . (x :|)
            . bool xs ("--show-trace" : xs)

containsAnyOf :: (Each s s a a, Foldable t, Eq a) => t a -> s -> Bool
containsAnyOf = anyOf each . flip elem

containsNoneOf :: (Each s s a a, Foldable t, Eq a) => t a -> s -> Bool
containsNoneOf = noneOf each . flip elem

isGitTracked :: Path b1 Dir -> Path b2 t2 -> App Bool
isGitTracked gitDir path =
    (ExitSuccess ==)
        <$> cmdCode
            ( sconcat
                [ ["git"]
                , ["-C", fromPath gitDir]
                , ["ls-files"]
                , ["--error-unmatch"]
                , [fromPath path]
                ]
            )

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

genM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t (a, b))
genM f = mapM \a -> (a,) <$> f a
