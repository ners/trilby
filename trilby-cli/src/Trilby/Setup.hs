module Trilby.Setup (ensureNix, ensureDeps) where

import Data.Text.IO qualified as Text
import Effectful.Path (overPath)
import Effectful.Path qualified as Path
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Trilby.Host (Host (..))
import Trilby.Prelude

nixBins
    :: (HasCallStack, Reader AppState :> es, TypedProcess :> es, Log :> es, FileSystem :> es)
    => [FileOrFlake]
    -> Eff es [Path Abs Dir]
nixBins fs = do
    outs <- concatMapM nixBuild fs
    filterM doesDirExist $ outs <&> (</> $(mkRelDir "bin"))

prependToPath :: (Environment :> es) => [Path Abs Dir] -> Eff es ()
prependToPath = overPath . (<>) . fmap toFilePath

installNix
    :: (HasCallStack, IOE :> es, Reader AppState :> es, Reader Host :> es, TypedProcess :> es, Log :> es)
    => Eff es ()
installNix = shell_ ["curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install | sh"]

ensureFlakes
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => Eff es ()
ensureFlakes = unlessM ((ExitSuccess ==) <$> cmdCode ["nix", "flake", "metadata", "nixpkgs"]) do
    confDir <- getXdgDir XdgConfig $ Just $(mkRelDir "nix")
    ensureDir confDir
    let confFile = confDir </> $(mkRelFile "nix.conf")
    liftIO . Text.appendFile (toFilePath confFile) $ "experimental-features = nix-command flakes"

setupNixMonitored
    :: ( HasCallStack
       , Environment :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => Eff es ()
setupNixMonitored = prependToPath =<< nixBins . pure . Flake =<< trilbyFlake ["nix-monitored"]

findBinary
    :: (HasCallStack, Environment :> es, FileSystem :> es, Log :> es)
    => String
    -> Eff es (Maybe FilePath)
findBinary c = do
    mp <- Path.findBinary c
    logTrace ("findBinary " <> fromString c) mp
    pure mp

ensureNix
    :: ( HasCallStack
       , IOE :> es
       , Environment :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => Eff es ()
ensureNix =
    findBinary "nix"
        >>= mapM parseAbsFile
        >>= mapM (fmap (toFilePath . filename) . canonicalizePath)
        >>= \case
            Just "nix-monitored" -> ensureFlakes
            Just "nix" -> ensureFlakes >> setupNixMonitored
            _ -> installNix >> ensureFlakes >> setupNixMonitored

ensureDeps
    :: ( HasCallStack
       , Environment :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , FileSystem :> es
       )
    => [(Text, Text)]
    -> Eff es ()
ensureDeps deps = do
    let ensure
            :: ( HasCallStack
               , Environment :> es
               , IOE :> es
               , Concurrent :> es
               , Reader AppState :> es
               , Reader Host :> es
               , TypedProcess :> es
               , Log :> es
               , FileSystem :> es
               )
            => (Text, Text)
            -> Eff es (Maybe FlakeRef)
        ensure (c, o) =
            findBinary (fromText c) >>= \case
                Nothing -> Just <$> trilbyFlake [o]
                Just _ -> pure Nothing
    flakes <- fmap Flake . catMaybes <$> mapM ensure deps
    prependToPath =<< nixBins flakes
