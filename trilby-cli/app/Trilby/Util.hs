module Trilby.Util where

import Control.Lens (Each (each), Getting, anyOf, noneOf, view, (^?))
import Control.Monad (zipWithM_)
import Control.Monad.Extra (ifM, whenM)
import Control.Monad.Logger (LogLevel (LevelDebug, LevelInfo), logDebug, logError, logInfo)
import Data.Bool (bool)
import Data.Char qualified as Char
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, nonEmpty)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Monoid (First)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Nix (prettyNix)
import Nix.TH (ToExpr (toExpr))
import Options.Applicative
import System.Console.ANSI
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (stderr, stdout)
import System.Posix (getEffectiveUserID)
import Text.Read (readMaybe)
import Trilby.App
import Turtle (isAbsolute)
import Turtle qualified
import UnliftIO (MonadIO (liftIO), askRunInIO, hFlush, readTVarIO)
import Prelude hiding (error, log, writeFile)

printError :: Text -> IO ()
printError t = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printWarn :: Text -> IO ()
printWarn t = do
    hSetSGR stderr [SetColor Foreground Vivid Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printInfo :: Text -> IO ()
printInfo t = do
    hSetSGR stderr [SetColor Foreground Dull Cyan]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printDebug :: Text -> IO ()
printDebug t = do
    hSetSGR stderr [SetColor Foreground Dull Magenta]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

prompt :: Text -> App Text
prompt message = liftIO do
    Text.putStr $ message <> " "
    hFlush stdout
    Text.getLine

askYesNo :: Text -> Bool -> App Bool
askYesNo question defaultValue = do
    answer <-
        prompt $
            question <> " " <> (if defaultValue then "[Y/n]" else "[y/N]")
    case Text.uncons answer of
        Nothing -> pure defaultValue
        Just (Char.toLower -> 'y', _) -> pure True
        Just (Char.toLower -> 'n', _) -> pure False
        _ -> do
            $(logError) "Invalid input"
            askYesNo question defaultValue

askChoice :: (Show a) => Text -> [a] -> App a
askChoice message values = do
    liftIO $ Text.putStrLn message
    zipWithM_ (\i v -> liftIO $ Text.putStrLn $ tshow i <> ") " <> tshow v) [1 :: Int ..] values
    selection <- readMaybe . Text.unpack <$> prompt ">"
    case selection of
        Just i | i > 0 && i <= length values -> pure $ values !! (i - 1)
        _ -> do
            $(logError) "Invalid input"
            askChoice message values

askEnum :: (Bounded a, Enum a, Show a) => Text -> App a
askEnum = flip askChoice [minBound .. maxBound]

errorExit :: Text -> App a
errorExit msg = $(logError) msg >> liftIO exitFailure

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

readsPrecBoundedEnumOn :: forall a. (Show a, Bounded a, Enum a) => (String -> String) -> Int -> String -> [(a, String)]
readsPrecBoundedEnumOn m _ s = maybeToList $ asum $ map f [minBound .. maxBound]
  where
    f e = (e,) <$> List.stripPrefix (m $ show e) (m s)

readsPrecBoundedEnum :: (Show a, Bounded a, Enum a) => Int -> String -> [(a, String)]
readsPrecBoundedEnum = readsPrecBoundedEnumOn id

fromText :: (IsString a) => Text -> a
fromText = fromString . Text.unpack

fromListSafe :: a -> [a] -> NonEmpty a
fromListSafe x xs = fromMaybe (fromList [x]) (nonEmpty xs)

prepend :: (Semigroup (f a), Applicative f) => a -> f a -> f a
prepend x xs = pure x <> xs

append :: (Semigroup (f a), Applicative f) => a -> f a -> f a
append x xs = xs <> pure x

cmd' :: NonEmpty Text -> App (ExitCode, Text)
cmd' (p :| args) = do
    $(logInfo) $ Text.unwords $ p : args
    Turtle.procStrict p args Turtle.stdin

cmd :: NonEmpty Text -> App Text
cmd args = do
    (code, out) <- cmd' args
    $(logDebug) $ "cmd return code: " <> tshow code
    case code of
        ExitSuccess -> pure out
        ExitFailure{} -> liftIO $ exitWith code

cmd_ :: NonEmpty Text -> App ()
cmd_ args = do
    out <- cmd args
    whenM (verbosityAtLeast LevelInfo) do
        liftIO $ Text.putStr out
        hFlush stdout

quietCmd_ :: NonEmpty Text -> App ()
quietCmd_ (p :| args) = do
    ifM (verbosityAtLeast LevelInfo) (cmd_ $ p :| args) do
        $(logInfo) $ Text.unwords $ p : args
        (code, _, err) <- Turtle.procStrictWithErr p args Turtle.stdin
        case code of
            ExitSuccess -> pure ()
            ExitFailure{} -> liftIO do
                Text.hPutStrLn stderr err
                exitWith code

isRoot :: App Bool
isRoot = (0 ==) <$> liftIO getEffectiveUserID

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

showNix :: (ToExpr e, IsString s) => e -> s
showNix = fromString . show . prettyNix . toExpr

ensureDir :: FilePath -> App ()
ensureDir "" = pure ()
ensureDir "." = pure ()
ensureDir d = bool id asRoot (isAbsolute d) cmd_ ["mkdir", "-p", fromString d]

inDir :: FilePath -> App a -> App a
inDir d a = do
    ensureDir d
    unlift <- askRunInIO
    liftIO $ Turtle.with (Turtle.pushd d) (const $ unlift a)

writeFile :: FilePath -> Text -> App ()
writeFile f t = do
    $(logDebug) $ "writeFile " <> fromString f
    Turtle.output f $ Turtle.toLines $ pure t

writeNixFile :: (ToExpr a) => FilePath -> a -> App ()
writeNixFile f = writeFile f . showNix

is :: a -> Getting (First c) a c -> Bool
is a c = isJust $ a ^? c

getVerbosity :: App LogLevel
getVerbosity = liftIO . readTVarIO =<< view #verbosity

verbosityAtLeast :: LogLevel -> App Bool
verbosityAtLeast v = (v >=) <$> getVerbosity

withTrace :: (NonEmpty Text -> App a) -> NonEmpty Text -> App a
withTrace f (x :| xs) =
    verbosityAtLeast LevelDebug
        >>= f . (x :|) . bool xs ("--show-trace" : xs)

containsAnyOf :: (Each s s a a, Foldable t, Eq a) => t a -> s -> Bool
containsAnyOf = anyOf each . flip elem

containsNoneOf :: (Each s s a a, Foldable t, Eq a) => t a -> s -> Bool
containsNoneOf = noneOf each . flip elem
