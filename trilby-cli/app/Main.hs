{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Nix.Prelude
import Nix.Pretty (prettyNix)
import Options.Applicative (execParser)
import Prettyprinter (LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), layoutPretty)
import Prettyprinter.Render.Text (renderIO)
import Trilby.Config
import Trilby.Disko
import Trilby.Install
import Trilby.Options
import Trilby.Update

main :: IO ()
main = do
    renderIO stdout $ layoutPretty (LayoutOptions $ AvailablePerLine 80 0.4) $ prettyNix $ disko defaultTrilbyConfig.disks

-- command <- execParser parseCommandInfo
-- case command of
--    Update opts -> update opts
--    Install opts -> install opts
