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
    , module UnliftIO
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
import Control.Monad.State (MonadState, evalStateT, execStateT, get, put)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Bifunctor qualified as Bifunctor
import Data.Bool
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Foldable
import Data.Functor
import Data.Generics.Labels ()
import Data.List.Extra (headDef, (!?))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe
import Data.Monoid (First)
import Data.Semigroup (sconcat)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable
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
    , canonicalizePath
    , doesDirExist
    , withSystemTempDir
    , withSystemTempFile
    )
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (BufferMode (NoBuffering), IO)
import System.Posix (getEffectiveUserID)
import System.Process qualified as Process
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (..), ReadPrec, readMaybe)
import Trilby.App (App)
import Turtle qualified
import UnliftIO hiding (withSystemTempDirectory, withSystemTempFile, withTempFile)
import UnliftIO.Environment
import "base" Prelude hiding (writeFile)

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

-- | Does not suppress a command's stdin, stdout, or stderr
rawCmdWith :: (HasCallStack) => NonEmpty Text -> Turtle.Shell Turtle.Line -> App ExitCode
rawCmdWith (p :| args) stdin = do
    logInfo . Text.unwords $ p : args
    Turtle.system
        ( (Process.proc (Text.unpack p) (Text.unpack <$> args))
            { Process.std_in = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            }
        )
        stdin

-- | Does not suppress a command's stdin, stdout, or stderr
rawCmd :: (HasCallStack) => NonEmpty Text -> App ExitCode
rawCmd = flip rawCmdWith empty

-- | Does not suppress a command's stdout or stderr
rawCmdWith_ :: (HasCallStack) => NonEmpty Text -> Turtle.Shell Turtle.Line -> App ()
rawCmdWith_ args stdin = do
    code <- rawCmdWith args stdin
    case code of
        ExitSuccess -> pure ()
        ExitFailure{} -> liftIO $ exitWith code

-- | Does not suppress a command's stdout or stderr
rawCmd_ :: (HasCallStack) => NonEmpty Text -> App ()
rawCmd_ = flip rawCmdWith_ empty

cmd' :: (HasCallStack) => NonEmpty Text -> App (ExitCode, Text)
cmd' (p :| args) = do
    logInfo . Text.unwords $ p : args
    Turtle.procStrict p args Turtle.stdin

cmd :: (HasCallStack) => NonEmpty Text -> App Text
cmd args = do
    (code, out) <- cmd' args
    logDebug $ "cmd: " <> ishow (code, out)
    case code of
        ExitSuccess -> pure out
        ExitFailure{} -> liftIO $ exitWith code

cmd_ :: (HasCallStack) => NonEmpty Text -> App ()
cmd_ args = do
    out <- cmd args
    whenM (verbosityAtLeast LevelInfo) do
        liftIO $ Text.putStr out
        hFlush stdout

-- | Suppresses a command's stderr if verbosity is not at least LevelInfo
quietCmd_ :: (HasCallStack) => NonEmpty Text -> App ()
quietCmd_ (p :| args) = ifM (verbosityAtLeast LevelInfo) (cmd_ $ p :| args) do
    logInfo . Text.unwords $ p : args
    (code, _, err) <- Turtle.procStrictWithErr p args Turtle.stdin
    case code of
        ExitSuccess -> pure ()
        ExitFailure{} -> liftIO do
            Text.hPutStrLn stderr err
            exitWith code

quietCmd' :: (HasCallStack) => NonEmpty Text -> App (ExitCode, Text)
quietCmd' (p :| args) = ifM (verbosityAtLeast LevelInfo) (cmd' $ p :| args) do
    logInfo . Text.unwords $ p : args
    (code, out, _) <- Turtle.procStrictWithErr p args Turtle.stdin
    pure (code, out)

isRoot :: App Bool
isRoot = (0 ==) <$> liftIO getEffectiveUserID

asUser :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asUser = id

asRoot :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
asRoot c t = ifM isRoot (c t) (c $ prepend "sudo" t)

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

shell' :: (HasCallStack) => Text -> Turtle.Shell Turtle.Line -> App (ExitCode, Text)
shell' cmd stdin = do
    logInfo cmd
    Turtle.systemStrict
        ( (Process.shell (Text.unpack cmd))
            { Process.std_in = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            }
        )
        stdin

shell :: (HasCallStack) => Text -> Turtle.Shell Turtle.Line -> App Text
shell cmd stdin = do
    (code, out) <- shell' cmd stdin
    logDebug $ "shell: " <> ishow (code, out)
    case code of
        ExitSuccess -> pure out
        ExitFailure{} -> liftIO $ exitWith code

shell_ :: (HasCallStack) => Text -> Turtle.Shell Turtle.Line -> App ()
shell_ cmd s = do
    out <- shell cmd s
    whenM (verbosityAtLeast LevelInfo) do
        liftIO $ Text.putStr out
        hFlush stdout

isAbsolute :: Path b t -> Bool
isAbsolute p = Just '/' == listToMaybe (toFilePath p)

isRelative :: Path b t -> Bool
isRelative = not . isAbsolute

ensureDir :: Path b Dir -> App ()
ensureDir dir = unlessM (doesDirExist dir) $ (if isAbsolute dir then asRoot else id) cmd_ ["mkdir", "-p", fromPath dir]

inDir :: Path b Dir -> App a -> App a
inDir d a = do
    ensureDir d
    unlift <- askRunInIO
    liftIO $ Turtle.with (Turtle.pushd $ toFilePath d) (const $ unlift a)

writeFile :: (HasCallStack) => Path b File -> Text -> App ()
writeFile f t = do
    logDebug $ "writeFile " <> fromPath f <> "\n" <> t
    ensureDir $ parent f
    Turtle.output (toFilePath f) $ Turtle.toLines $ pure t

withTempFile :: Path Rel File -> (Path Abs File -> App a) -> App a
withTempFile file action = do
    tmpDir <- view #tmpDir
    let tmpFile = tmpDir </> file
    withFile (toFilePath tmpFile) ReadWriteMode \handle -> do
        hClose handle
        action tmpFile

getSymlinkTarget :: (MonadIO m) => (FilePath -> m (Path b2 t2)) -> SomeBase File -> m (Path b2 t2)
getSymlinkTarget = (Turtle.realpath . fromSomeBase >=>)

is :: a -> Getting (First c) a c -> Bool
is a c = isJust $ a ^? c

verbosityAtLeast :: LogLevel -> App Bool
verbosityAtLeast v = (v >=) <$> view #verbosity

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
