module Trilby.Config.Size where

import Data.Text qualified as Text
import Numeric (showFFloat)
import Trilby.Prelude

format :: Integer -> Text
format bytes
    | bytes < 1000 = ishow bytes <> " B"
    | otherwise = scale (fromInteger bytes / 1000) ["kB", "MB", "GB"] "TB"

scale :: Double -> [Text] -> Text -> Text
scale value (unit : larger) largest
    | value < 999.95 = display value unit
    | otherwise = scale (value / 1000) larger largest
scale value [] largest = display value largest

display :: Double -> Text -> Text
display value unit = Text.pack (showFFloat (Just 1) value "") <> " " <> unit
