module Trilby.Util where

import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char qualified as Char
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Nix (prettyNix)
import Nix.TH (ToExpr (toExpr))
import Options.Applicative (Parser, flag', help, long, (<|>))
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (hFlush, stderr, stdout)
import Text.Read (readMaybe)
import Turtle qualified
import Prelude hiding (error, log)

error :: (MonadIO m) => Text -> m ()
error t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

warn :: (MonadIO m) => Text -> m ()
warn t = liftIO do
    hSetSGR stderr [SetColor Foreground Dull Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

info :: (MonadIO m) => Text -> m ()
info t = liftIO do
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
            error "Unrecognised input"
            ask question defaultValue

choose :: (Show a, MonadIO m) => Text -> [a] -> m a
choose message values = liftIO do
    Text.putStrLn message
    zipWithM_ (\i v -> Text.putStrLn $ tshow i <> ") " <> tshow v) [1 :: Int ..] values
    selection <- readMaybe . Text.unpack <$> prompt ">"
    case selection of
        Just i | i > 0 && i <= length values -> pure $ values !! (i - 1)
        _ -> do
            error "Invalid input"
            choose message values

errorExit :: (MonadIO m) => Text -> m ()
errorExit msg = error msg >> liftIO exitFailure

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

fromText :: (IsString a) => Text -> a
fromText = fromString . Text.unpack

fromListSafe :: a -> [a] -> NonEmpty a
fromListSafe x xs = fromMaybe (fromList [x]) (nonEmpty xs)

shell :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m Turtle.ExitCode
shell cmd input = info cmd >> Turtle.shell cmd input

shell_ :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m ()
shell_ cmd input = info cmd >> Turtle.shells cmd input

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

showNix :: (ToExpr e, IsString s) => e -> s
showNix = fromString . show . prettyNix . toExpr

writeNixFile :: (ToExpr a, MonadIO m) => FilePath -> a -> m ()
writeNixFile f = Turtle.output f . Turtle.toLines . pure . showNix
