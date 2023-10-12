module Trilby.Util where

import Control.Lens (Getting, (^?))
import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty)
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
import System.Exit (exitFailure)
import System.IO (hFlush, stderr, stdout)
import Text.Read (readMaybe)
import Turtle (dirname)
import Turtle qualified
import Prelude hiding (error, log)

logerror :: (MonadIO m) => Text -> m ()
logerror t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

logwarn :: (MonadIO m) => Text -> m ()
logwarn t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

loginfo :: (MonadIO m) => Text -> m ()
loginfo t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull White]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

prompt :: (MonadIO m) => Text -> m Text
prompt message = liftIO do
    Text.putStr $ message <> " "
    hFlush stdout
    Text.getLine

ask :: (MonadIO m) => Text -> Bool -> m Bool
ask question defaultValue = do
    answer <-
        prompt $
            question <> " " <> (if defaultValue then "[Y/n]" else "[y/N]")
    case Text.uncons answer of
        Nothing -> pure defaultValue
        Just (Char.toLower -> 'y', _) -> pure True
        Just (Char.toLower -> 'n', _) -> pure False
        _ -> do
            logerror "Unrecognised input"
            ask question defaultValue

askChoice :: (Show a, MonadIO m) => Text -> [a] -> m a
askChoice message values = liftIO do
    Text.putStrLn message
    zipWithM_ (\i v -> Text.putStrLn $ tshow i <> ") " <> tshow v) [1 :: Int ..] values
    selection <- readMaybe . Text.unpack <$> prompt ">"
    case selection of
        Just i | i > 0 && i <= length values -> pure $ values !! (i - 1)
        _ -> do
            logerror "Invalid input"
            askChoice message values

askEnum :: (Bounded a, Enum a, Show a, MonadIO m) => Text -> m a
askEnum = flip askChoice [minBound .. maxBound]

errorExit :: (MonadIO m) => Text -> m a
errorExit msg = logerror msg >> liftIO exitFailure

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

readsPrecBoundedEnumOn :: (Show a, Bounded a, Enum a) => (String -> String) -> Int -> String -> [(a, String)]
readsPrecBoundedEnumOn m _ s = f [minBound .. maxBound]
  where
    f = maybeToList . asum . fmap g
    g e = (e,) <$> List.stripPrefix (m $ show e) (m s)

readsPrecBoundedEnum :: (Show a, Bounded a, Enum a) => Int -> String -> [(a, String)]
readsPrecBoundedEnum = readsPrecBoundedEnumOn id

fromText :: (IsString a) => Text -> a
fromText = fromString . Text.unpack

fromListSafe :: a -> [a] -> NonEmpty a
fromListSafe x xs = fromMaybe (fromList [x]) (nonEmpty xs)

shell :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m Turtle.ExitCode
shell cmd input = loginfo cmd >> Turtle.shell cmd input

shell_ :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m ()
shell_ cmd input = loginfo cmd >> Turtle.shells cmd input

sudo :: (MonadIO m) => Text -> m Turtle.ExitCode
sudo cmd = shell ("sudo " <> cmd) Turtle.stdin

sudo_ :: (MonadIO m) => Text -> m ()
sudo_ cmd = shell_ ("sudo " <> cmd) Turtle.stdin

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

inshellstrict :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m Text
inshellstrict cmd = Turtle.strict . Turtle.inshell cmd

parseYesNo :: String -> String -> (Parser Bool -> Parser (m Bool)) -> Parser (m Bool)
parseYesNo yesLong yesHelp f = f $ flag' True (long yesLong <> help yesHelp) <|> flag' False (long noLong)
  where
    noLong = "no-" <> yesLong

parseEnum :: forall a m. (Bounded a, Enum a, Show a, Read a) => Mod OptionFields a -> (Parser a -> Parser (m a)) -> Parser (m a)
parseEnum m f = f $ option (maybeReader readMaybe) $ m <> showDefault <> completeWith options
  where
    options = show @a <$> [minBound .. maxBound]

showNix :: (ToExpr e, IsString s) => e -> s
showNix = fromString . show . prettyNix . toExpr

ensureDir :: (MonadIO m) => FilePath -> m ()
ensureDir d = shell_ ("mkdir -p " <> fromString d) empty

writeNixFile :: (ToExpr a, MonadIO m) => FilePath -> a -> m ()
writeNixFile f a = do
    ensureDir $ dirname f
    Turtle.output f . Turtle.toLines . pure $ showNix a

is :: a -> Getting (First c) a c -> Bool
is a c = isJust $ a ^? c
