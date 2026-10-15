module Trilby.Config.Progress where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful.Concurrent.MVar (MVar, modifyMVar, newMVar)
import System.Console.ANSI
    ( Color (Yellow)
    , ColorIntensity (Dull)
    , ConsoleLayer (Foreground)
    , SGR (Reset, SetColor)
    , hClearLine
    , hCursorDown
    , hCursorUp
    , hSetCursorColumn
    , hSupportsANSI
    , setSGRCode
    )
import System.IO (stderr)
import Trilby.Prelude

data Progress = Progress
    { colour :: Bool
    , redraw :: Bool
    , printed :: MVar Int
    }

maxRedrawDistance :: Int
maxRedrawDistance = 15

new :: (HasCallStack, IOE :> es, Concurrent :> es, Reader AppState :> es) => Eff es Progress
new = do
    colour <- liftIO $ hSupportsANSI stderr
    redraw <- (colour &&) . not <$> verbosityAtLeast LogInfo
    printed <- newMVar 0
    pure Progress{..}

exclusively :: (Concurrent :> es) => Progress -> (Int -> Eff es (Int, a)) -> Eff es a
exclusively Progress{printed} = modifyMVar printed

styled :: Progress -> Text -> Text
styled Progress{colour} message
    | colour =
        Text.pack (setSGRCode [SetColor Foreground Dull Yellow])
            <> message
            <> Text.pack (setSGRCode [Reset])
    | otherwise = message

start :: (IOE :> es, Concurrent :> es) => Progress -> Text -> Eff es Int
start progress message = exclusively progress \count -> do
    liftIO . Text.hPutStrLn stderr $ styled progress message
    pure (count + 1, count)

finish :: (IOE :> es, Concurrent :> es) => Progress -> Int -> Text -> Eff es ()
finish progress@Progress{redraw} line message = exclusively progress \count -> liftIO do
    let distance = count - line
    if redraw && distance <= maxRedrawDistance
        then do
            hCursorUp stderr distance
            hClearLine stderr
            Text.hPutStr stderr $ styled progress message
            hCursorDown stderr distance
            hSetCursorColumn stderr 0
            pure (count, ())
        else do
            Text.hPutStrLn stderr $ styled progress message
            pure (count + 1, ())
