{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Data.Fix (Fix (Fix))
import Nix
import Nix.Prelude
import Nix.TH (ToExpr (toExpr))
import Options.Applicative (execParser)
import Prettyprinter (LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), layoutPretty)
import Prettyprinter.Render.Text (renderIO)
import Trilby.Disko
import Trilby.HNix
import Trilby.Install
import Trilby.Options
import Trilby.Update

main :: IO ()
main = do
    renderIO stdout $
        layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4) $
            prettyNix $ unAnnotate $ toExpr defaultDisko

    command <- execParser parseCommandInfo
    case command of
        Update opts -> update opts
        Install opts -> install opts
