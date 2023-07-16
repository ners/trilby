module Trilby.Util where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char qualified as Char
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (hFlush, stderr, stdout)
import Turtle qualified as Turtle
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
            question <> " " <> case defaultValue of
                True -> "[Y/n]"
                False -> "[y/N]"
    case Text.uncons answer of
        Nothing -> pure defaultValue
        Just (Char.toLower -> 'y', _) -> pure True
        Just (Char.toLower -> 'n', _) -> pure False
        _ -> do
            error "Unrecognised input"
            ask question defaultValue

errorExit :: (MonadIO m) => Text -> m ()
errorExit msg = error msg >> liftIO exitFailure

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

fromText :: (IsString a) => Text -> a
fromText = fromString . Text.unpack

shell :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m Turtle.ExitCode
shell cmd input = info cmd >> Turtle.shell cmd input

shells :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m ()
shells cmd input = info cmd >> Turtle.shells cmd input

sudo :: (MonadIO m) => Text -> m ()
sudo = flip shells Turtle.stdin . ("sudo " <>)
