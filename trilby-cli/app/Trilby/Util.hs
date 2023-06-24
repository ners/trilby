module Trilby.Util where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Shelly
import System.Console.ANSI
import System.IO (stderr)
import Prelude hiding (error)

error :: Text -> IO ()
error t = do
    hSetSGR stderr [SetColor Foreground Dull Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

warn :: Text -> IO ()
warn t = do
    hSetSGR stderr [SetColor Foreground Dull Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

info :: Text -> IO ()
info t = do
    hSetSGR stderr [SetColor Foreground Dull White]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

peval :: (Text -> IO ()) -> Sh a -> Sh a
peval f = print_commands True . print_commands_with f

ask :: String -> Bool -> Sh Bool
ask question defaultValue = do
    answer <- liftIO $ do
        putStrLn question
        getLine
    case answer of
        "" -> pure defaultValue
        'y' : _ -> pure True
        'Y' : _ -> pure True
        'n' : _ -> pure False
        'N' : _ -> pure False
        _ -> do
            liftIO $ error "unrecognised input"
            ask question defaultValue

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
