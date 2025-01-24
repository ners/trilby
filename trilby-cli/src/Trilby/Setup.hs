module Trilby.Setup (setup) where

import Data.Text qualified as Text
import Path.IO (XdgDirectory (XdgConfig), getXdgDir)
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Turtle qualified
import Prelude

appendToPath :: [Path Abs Dir] -> App ()
appendToPath fs = do
    currentPath <- Turtle.need "PATH"
    let newPath = Text.intercalate ":" . catMaybes $ [Just . Text.pack . toFilePath $ f | f <- fs] <> [currentPath]
    Turtle.export "PATH" newPath

appendNixBinsToPath :: [FileOrFlake] -> App ()
appendNixBinsToPath fs = do
    outs <- concatMapM nixBuild fs
    bins <- filterM doesDirExist $ outs <&> (</> $(mkRelDir "bin"))
    appendToPath bins

installNix :: App ()
installNix =
    flip shell_ Turtle.empty . Text.intercalate " | " $
        [ "curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix"
        , "sh -s -- install --no-confirm"
        ]

ensureFlakes :: App ()
ensureFlakes = unlessM ((ExitSuccess ==) . fst <$> quietCmd' ["nix", "flake", "metadata", "nixpkgs"]) do
    confDir <- getXdgDir XdgConfig $ Just $(mkRelDir "nix")
    ensureDir confDir
    let confFile = confDir </> $(mkRelFile "nix.conf")
    Turtle.append (toFilePath confFile) . Turtle.toLines $ "experimental-features = nix-command flakes"

setupNixMonitored :: App ()
setupNixMonitored = appendNixBinsToPath . pure . Flake =<< trilbyFlake ["nix-monitored"]

ensureNix :: App ()
ensureNix =
    Turtle.which "nix" >>= mapM (fmap Turtle.basename . Turtle.realpath) >>= \case
        Just "nix-monitored" -> ensureFlakes
        Just "nix" -> ensureFlakes >> setupNixMonitored
        _ -> installNix >> ensureFlakes >> setupNixMonitored

ensureDeps :: App ()
ensureDeps = do
    let ensure :: String -> [Text] -> App (Maybe FlakeRef)
        ensure c o =
            Turtle.which c >>= \case
                Nothing -> Just <$> trilbyFlake o
                Just _ -> pure Nothing
    flakes <-
        fmap catMaybes . sequence $
            [ ensure "nvd" ["nvd"]
            , ensure "unbuffer" ["expect"]
            ]
    appendNixBinsToPath $ Flake <$> flakes

setup :: App ()
setup = do
    ensureNix
    ensureDeps
