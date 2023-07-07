module Trilby.Util where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Console.ANSI
import System.Exit (exitFailure)
import System.IO (hFlush, stderr, stdout)
import Turtle qualified as Turtle
import Turtle.Prelude qualified as Turtle
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

ask :: (MonadIO m) => String -> Bool -> m Bool
ask question defaultValue = do
    answer <- liftIO do
        putStr $
            question <> case defaultValue of
                True -> " [Y/n] "
                False -> " [y/N] "
        hFlush stdout
        getLine
    case answer of
        "" -> pure defaultValue
        'y' : _ -> pure True
        'Y' : _ -> pure True
        'n' : _ -> pure False
        'N' : _ -> pure False
        _ -> do
            error "unrecognised input"
            ask question defaultValue

errorExit :: (MonadIO m) => Text -> m ()
errorExit msg = error msg >> liftIO exitFailure

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

shell :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m Turtle.ExitCode
shell cmd input = info cmd >> Turtle.shell cmd input

shells :: (MonadIO m) => Text -> Turtle.Shell Turtle.Line -> m ()
shells cmd input = info cmd >> Turtle.shells cmd input

sudo :: (MonadIO m) => Text -> m ()
sudo = flip shells Turtle.stdin . ("sudo " <>)
