module Trilby.Setup (ensureNix, ensureDeps) where

import Data.Text.IO qualified as Text
import Effectful.Path (overPath)
import Effectful.Path qualified as Path
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Trilby.Prelude

nixBins :: (HasCallStack) => [FileOrFlake] -> App [Path Abs Dir]
nixBins fs = do
    outs <- concatMapM nixBuild fs
    filterM doesDirExist $ outs <&> (</> $(mkRelDir "bin"))

prependToPath :: [Path Abs Dir] -> App ()
prependToPath = overPath . (<>) . fmap toFilePath

installNix :: (HasCallStack) => App ()
installNix = shell_ ["curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install | sh"]

ensureFlakes :: (HasCallStack) => App ()
ensureFlakes = unlessM ((ExitSuccess ==) <$> cmdCode ["nix", "flake", "metadata", "nixpkgs"]) do
    confDir <- getXdgDir XdgConfig $ Just $(mkRelDir "nix")
    ensureDir confDir
    let confFile = confDir </> $(mkRelFile "nix.conf")
    liftIO . Text.appendFile (toFilePath confFile) $ "experimental-features = nix-command flakes"

setupNixMonitored :: (HasCallStack) => App ()
setupNixMonitored = prependToPath =<< nixBins . pure . Flake =<< trilbyFlake ["nix-monitored"]

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

ensureDeps :: (HasCallStack) => [(Text, Text)] -> App ()
ensureDeps deps = do
    let ensure :: (HasCallStack) => (Text, Text) -> App (Maybe FlakeRef)
        ensure (c, o) =
            findBinary (fromText c) >>= \case
                Nothing -> Just <$> trilbyFlake [o]
                Just _ -> pure Nothing
    flakes <- fmap Flake . catMaybes <$> mapM ensure deps
    prependToPath =<< nixBins flakes
