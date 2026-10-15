module Trilby.Setup (ensureNix, ensureDeps) where

import Data.Text.IO qualified as Text
import Effectful.Path (overPath)
import Effectful.Path qualified as Path
import Trilby.HNix (FileOrFlake (Flake), nixBuild, trilbyFlake)
import Trilby.Host (Host (..))
import Trilby.Prelude
import Trilby.Widgets (yesNoButtons)

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
    :: ( HasCallStack
       , IOE :> es
       , Environment :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Eff es ()
installNix = do
    whenM isOffline $ errorExit "Nix is required but not installed"
    unlessM (yesNoButtons "Nix is required but not installed. Download and run the Nix installer?" True)
        $ errorExit "Cannot continue without Nix"
    shell_
        [ "set -e; f=$(mktemp); trap 'rm -f \"$f\"' EXIT;"
        , "curl --proto '=https' --tlsv1.2 -fL -o \"$f\" https://nixos.org/nix/install;"
        , "sh \"$f\""
        ]
    prependToPath [$(mkAbsDir "/nix/var/nix/profiles/default/bin")]

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
ensureFlakes = unlessM ((ExitSuccess ==) <$> (cmdCode =<< nixCommand ["flake", "metadata", "nixpkgs"])) do
    confDir <- getXdgDir XdgConfig $ Just $(mkRelDir "nix")
    ensureDir confDir
    let confFile = confDir </> $(mkRelFile "nix.conf")
    liftIO . Text.appendFile (toFilePath confFile) $ "\nexperimental-features = nix-command flakes\n"

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
setupNixMonitored =
    ifM
        isOffline
        (logInfo_ "Offline, not setting up nix-monitored")
        (prependToPath =<< nixBins . pure . Flake =<< trilbyFlake ["nix-monitored"])

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
