module Trilby.Setup (setup) where

import Data.List qualified as List
import Data.Text.IO qualified as Text
import Effectful.Path (findBinary)
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Prelude

appendToPath :: [Path Abs Dir] -> App ()
appendToPath fs = do
    currentPath <- lookupEnv "PATH"
    let newPath = List.intercalate ":" $ (toFilePath <$> fs) <> maybeToList currentPath
    setEnv "PATH" newPath

appendNixBinsToPath :: (HasCallStack) => [FileOrFlake] -> App ()
appendNixBinsToPath fs = do
    outs <- concatMapM nixBuild fs
    bins <- filterM doesDirExist $ outs <&> (</> $(mkRelDir "bin"))
    appendToPath bins

installNix :: (HasCallStack) => App ()
installNix = shell_ ["curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --no-confirm"]

ensureFlakes :: (HasCallStack) => App ()
ensureFlakes = unlessM ((ExitSuccess ==) <$> cmdCode ["nix", "flake", "metadata", "nixpkgs"]) do
    confDir <- getXdgDir XdgConfig $ Just $(mkRelDir "nix")
    ensureDir confDir
    let confFile = confDir </> $(mkRelFile "nix.conf")
    liftIO . Text.appendFile (toFilePath confFile) $ "experimental-features = nix-command flakes"

setupNixMonitored :: (HasCallStack) => App ()
setupNixMonitored = appendNixBinsToPath . pure . Flake =<< trilbyFlake ["nix-monitored"]

ensureNix :: (HasCallStack) => App ()
ensureNix =
    findBinary "nix"
        >>= mapM parseAbsFile
        >>= mapM (fmap (toFilePath . filename) . canonicalizePath)
        >>= \case
            Just "nix-monitored" -> ensureFlakes
            Just "nix" -> ensureFlakes >> setupNixMonitored
            _ -> installNix >> ensureFlakes >> setupNixMonitored

ensureDeps :: (HasCallStack) => App ()
ensureDeps = do
    let ensure :: (HasCallStack) => String -> [Text] -> App (Maybe FlakeRef)
        ensure c o =
            findBinary c >>= \case
                Nothing -> Just <$> trilbyFlake o
                Just _ -> pure Nothing
    flakes <-
        fmap catMaybes . sequence $
            [ ensure "nvd" ["nvd"]
            , ensure "unbuffer" ["expect"]
            ]
    appendNixBinsToPath $ Flake <$> flakes

setup :: (HasCallStack) => App ()
setup = do
    ensureNix
    ensureDeps
