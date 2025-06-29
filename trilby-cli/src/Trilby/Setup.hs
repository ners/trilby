module Trilby.Setup (setup) where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Path.IO (XdgDirectory (XdgConfig), getXdgDir)
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Prelude

appendToPath :: [Path Abs Dir] -> App ()
appendToPath fs = do
    currentPath <- fromString <$$> lookupEnv "PATH"
    let newPath = Text.intercalate ":" . catMaybes $ [Just . Text.pack . toFilePath $ f | f <- fs] <> [currentPath]
    setEnv "PATH" $ fromText newPath

appendNixBinsToPath :: [FileOrFlake] -> App ()
appendNixBinsToPath fs = do
    outs <- concatMapM nixBuild fs
    bins <- filterM doesDirExist $ outs <&> (</> $(mkRelDir "bin"))
    appendToPath bins

installNix :: App ()
installNix =
    runShell . Text.intercalate " | " $
        [ "curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix"
        , "sh -s -- install --no-confirm"
        ]

ensureFlakes :: App ()
ensureFlakes = unlessM ((ExitSuccess ==) . fst <$> readProcess' ["nix", "flake", "metadata", "nixpkgs"]) do
    confDir <- getXdgDir XdgConfig $ Just $(mkRelDir "nix")
    ensureDir confDir
    let confFile = confDir </> $(mkRelFile "nix.conf")
    liftIO $ Text.appendFile (toFilePath confFile) "experimental-features = nix-command flakes"

setupNixMonitored :: App ()
setupNixMonitored = appendNixBinsToPath . pure . Flake =<< trilbyFlake ["nix-monitored"]

ensureNix :: App ()
ensureNix = do
    realNix <- mapM canonicalizePath =<< which "nix"
    case toFilePath . filename <$> realNix of
        "nix-monitored" : _ -> ensureFlakes
        "nix" : _ -> ensureFlakes >> setupNixMonitored
        _ -> installNix >> ensureFlakes >> setupNixMonitored

ensureDeps :: App ()
ensureDeps = do
    let ensure :: FilePath -> [Text] -> App (Maybe FlakeRef)
        ensure c o =
            which c >>= \case
                [] -> Just <$> trilbyFlake o
                _ -> pure Nothing
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
