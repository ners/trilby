module Prelude
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.Extra
    , module Control.Monad.Logger
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans
    , module Data.Bool
    , module Data.Char
    , module Data.Default
    , module Data.Foldable
    , module Data.List.Extra
    , module Data.List.NonEmpty
    , module Data.Maybe
    , module Data.Semigroup
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    , module GHC.IsList
    , module Nix.Expr.Types
    , module Nix.Pretty
    , module Nix.TH
    , module Prelude
    , module System.Exit
    , module System.IO
    , module Trilby.App
    , module UnliftIO
    )
where

import Control.Applicative
import Control.Arrow hiding (loop)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Logger (LogLevel (..), logDebug, logError, logInfo, logWarn)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, evalStateT, execStateT, get, put)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Bool
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Foldable
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.List.Extra (headDef, (!?))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe
import Data.Monoid (First)
import Data.Semigroup (sconcat)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import GHC.IsList hiding (toList)
import Nix.Expr.Types
import Nix.Pretty (prettyNix)
import Nix.TH (ToExpr (toExpr), nix)
import Options.Applicative
    ( Mod
    , OptionFields
    , Parser
    , completeWith
    , flag'
    , help
    , long
    , maybeReader
    , option
    , showDefault
    , showDefaultWith
    )
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (BufferMode (NoBuffering), IO)
import System.Posix (fileExist, getEffectiveUserID)
import System.Process qualified as Process
import Text.Read qualified as Text
import Trilby.App (App)
import Turtle (system)
import Turtle qualified
import UnliftIO
import "base" Prelude hiding (writeFile)

parseYesNo :: String -> String -> Parser Bool
parseYesNo yesLong yesHelp = flag' True (long yesLong <> help yesHelp) <|> flag' False (long noLong)
  where
    noLong = "no-" <> yesLong

parseChoiceWith :: forall a. (a -> String) -> (String -> Maybe a) -> Mod OptionFields a -> [a] -> Parser a
parseChoiceWith show' read' m xs = option (maybeReader read') $ m <> showDefaultWith show' <> completeWith options
  where
    options = show' <$> xs

parseChoice :: forall a. (Show a, Read a) => Mod OptionFields a -> [a] -> Parser a
parseChoice m xs = option (maybeReader Text.readMaybe) $ m <> showDefault <> completeWith options
  where
    options = show @a <$> xs

parseEnum :: (Bounded a, Enum a, Show a, Read a) => Mod OptionFields a -> Parser a
parseEnum = flip parseChoice [minBound .. maxBound]

errorExit :: Text -> App a
errorExit msg = $(logError) msg >> liftIO exitFailure
{-# INLINE errorExit #-}

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

readsPrecBoundedEnumOn :: forall a. (Show a, Bounded a, Enum a) => (String -> String) -> Int -> String -> [(a, String)]
readsPrecBoundedEnumOn m _ s = maybeToList $ asum $ map f [minBound .. maxBound]
  where
    f e = (e,) <$> List.stripPrefix (m $ show e) (m s)

readsPrecBoundedEnum :: (Show a, Bounded a, Enum a) => Int -> String -> [(a, String)]
readsPrecBoundedEnum = readsPrecBoundedEnumOn id

fromText :: (IsString s) => Text -> s
fromText = fromString . Text.unpack

fromListSafe :: a -> [a] -> NonEmpty a
fromListSafe x = fromMaybe (x :| []) . nonEmpty

firstLine :: Text -> Text
firstLine = headDef "" . Text.lines

prepend :: (Semigroup (f a), Applicative f) => a -> f a -> f a
prepend x xs = pure x <> xs

append :: (Semigroup (f a), Applicative f) => a -> f a -> f a
append x xs = xs <> pure x

-- | Does not suppress a command's stdin, stdout, or stderr
rawCmd :: NonEmpty Text -> App ExitCode
rawCmd (p :| args) = do
    $(logInfo) $ Text.unwords $ p : args
    system
        ( (Process.proc (Text.unpack p) (Text.unpack <$> args))
            { Process.std_in = Process.Inherit
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            }
        )
        empty

-- | Does not suppress a command's stdout or stderr
rawCmd_ :: NonEmpty Text -> App ()
rawCmd_ args = do
    code <- rawCmd args
    case code of
        ExitSuccess -> pure ()
        ExitFailure{} -> liftIO $ exitWith code

cmd' :: NonEmpty Text -> App (ExitCode, Text)
cmd' (p :| args) = do
    $(logInfo) $ Text.unwords $ p : args
    Turtle.procStrict p args Turtle.stdin

cmd :: NonEmpty Text -> App Text
cmd args = do
    (code, out) <- cmd' args
    $(logDebug) $ "cmd return code: " <> ishow code
    case code of
        ExitSuccess -> pure out
        ExitFailure{} -> liftIO $ exitWith code

cmd_ :: NonEmpty Text -> App ()
cmd_ args = do
    out <- cmd args
    whenM (verbosityAtLeast LevelInfo) do
        liftIO $ Text.putStr out
        hFlush stdout

-- | Suppresses a command's stderr if verbosity is not at least LevelInfo
quietCmd_ :: NonEmpty Text -> App ()
quietCmd_ (p :| args) = ifM (verbosityAtLeast LevelInfo) (cmd_ $ p :| args) do
    $(logInfo) $ Text.unwords $ p : args
    (code, _, err) <- Turtle.procStrictWithErr p args Turtle.stdin
    case code of
        ExitSuccess -> pure ()
        ExitFailure{} -> liftIO do
            Text.hPutStrLn stderr err
            exitWith code

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

shell :: Text -> Turtle.Shell Turtle.Line -> App Text
shell t = Turtle.strict . Turtle.inshell t

proc :: NonEmpty Text -> Turtle.Shell Turtle.Line -> App Text
proc (p :| args) = Turtle.strict . Turtle.inproc p args

ensureDir :: FilePath -> App ()
ensureDir d = unlessM (liftIO $ fileExist d) $ bool id asRoot (Turtle.isAbsolute d) cmd_ ["mkdir", "-p", fromString d]

inDir :: FilePath -> App a -> App a
inDir d a = do
    ensureDir d
    unlift <- askRunInIO
    liftIO $ Turtle.with (Turtle.pushd d) (const $ unlift a)

writeFile :: FilePath -> Text -> App ()
writeFile f t = do
    $(logDebug) $ "writeFile " <> fromString f
    Turtle.output f $ Turtle.toLines $ pure t

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
