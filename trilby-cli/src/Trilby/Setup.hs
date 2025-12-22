module Trilby.Setup (setup) where

import Data.List qualified as List
import Data.Text.IO qualified as Text
import Effectful.Path qualified as Path
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Trilby.Prelude

appendToPath :: [Path Abs Dir] -> App ()
appendToPath fs = do
    logInfo "Appending to PATH" fs
    currentPath <- lookupEnv "PATH"
    let newPath = maybeToList currentPath <> (toFilePath <$> fs)
    logInfo "New PATH:" fs
    setEnv "PATH" $ List.intercalate ":" newPath

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

findBinary :: String -> App (Maybe FilePath)
findBinary c = do
    mp <- Path.findBinary c
    logTrace ("findBinary " <> fromString c) mp
    pure mp

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
        fmap catMaybes
            . sequence
            $ [ ensure "mkpasswd" ["mkpasswd"]
              , ensure "nvd" ["nvd"]
              , ensure "unbuffer" ["expect"]
              , ensure "disko" ["disko"]
              ]
    appendNixBinsToPath $ Flake <$> flakes

setup :: (HasCallStack) => App ()
setup = do
    ensureNix
    ensureDeps
