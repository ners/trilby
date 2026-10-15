module Trilby.Config.Editor where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful.FileSystem.Path.IO (doesFileExist)
import Trilby.Config.Apply qualified as Apply
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.File qualified as File
import Trilby.Config.Options (EditOpts (..), Selection (..))
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Target (Destination (..))
import Trilby.Config.Target qualified as Target
import Trilby.Prelude

data Resolved = Resolved
    { file :: Path Abs File
    , missing :: Bool
    }

editorCommand :: Maybe Text -> Maybe Text -> NonEmpty Text
editorCommand visual editor =
    fromMaybe ("nano" :| [])
        . nonEmpty
        . Text.words
        . fromMaybe ""
        $ find (not . Text.null . Text.strip) (catMaybes [visual, editor])

profileTemplate :: NExpr
profileTemplate = [nix| { pkgs, ... }: { } |]

readText :: (Configuring es) => Path Abs File -> Eff es Text
readText = liftIO . Text.readFile . fromPath

profileTarget :: (Configuring es) => Path Abs Dir -> Text -> Eff es Resolved
profileTarget flake name = do
    plain <- Target.relativeFile flake $ "profiles/" <> name <> ".nix"
    directory <- Target.relativeFile flake $ "profiles/" <> name <> "/default.nix"
    existing <- findM doesFileExist [plain, directory]
    pure Resolved{file = fromMaybe plain existing, missing = isNothing existing}

userTarget :: (Configuring es) => Path Abs Dir -> Selector.Selector -> Eff es Resolved
userTarget flake selector =
    Selector.select selector <$> Target.localUsers flake >>= \case
        [user] -> do
            file <- Target.relativeFile flake $ "users/" <> user <> "/default.nix"
            unlessM (doesFileExist file) . errorExit $ "No such user: " <> user
            pure Resolved{missing = False, ..}
        [] -> errorExit "No such user, create one with `trilby config users add`"
        several -> errorExit $ "Choose a single user, matching users: " <> Text.unwords several

hostTarget
    :: (Configuring es) => Path Abs Dir -> Eval.Inventory -> Maybe Selector.Selector -> Eff es Resolved
hostTarget flake inventory selector =
    Target.hostNames inventory selector >>= \case
        [host] -> do
            Destination{file} <- Target.hostFile flake host
            pure Resolved{missing = False, ..}
        several -> errorExit $ "Choose a single host, matching hosts: " <> Text.unwords several

fileTarget :: (Configuring es) => Path Abs Dir -> FilePath -> Eff es Resolved
fileTarget flake path = do
    Destination{file} <- Target.explicitFile flake path
    pure Resolved{missing = False, ..}

resolve :: (Configuring es) => Selection -> Maybe FilePath -> Eval.Inventory -> Eff es Resolved
resolve Selection{flake, hosts, users, profile} file inventory = do
    when (length (catMaybes [void hosts, void users, void profile, void file]) > 1)
        $ errorExit "Choose only one of --host, --user, --profile and --file"
    case (file, profile, users) of
        (Just path, _, _) -> fileTarget flake path
        (_, Just name, _) -> profileTarget flake name
        (_, _, Just selector) -> userTarget flake selector
        _ -> hostTarget flake inventory hosts

openEditor :: (Configuring es) => Path Abs File -> Eff es ()
openEditor file = do
    visual <- lookupEnv "VISUAL"
    editor <- lookupEnv "EDITOR"
    runProcess_
        . proc
        $ editorCommand (fromString <$> visual) (fromString <$> editor)
        <> [fromPath file]

edit :: (Configuring es) => EditOpts Maybe -> Eff es ()
edit EditOpts{selection = selection@Selection{flake}, file, apply} = do
    inventory <- Eval.inventory flake
    Resolved{file = target, missing} <- resolve selection file inventory
    when missing $ File.create target profileTemplate
    original <- readText target
    openEditor target
    changed <- (original /=) <$> readText target
    if changed
        then Target.hostNames inventory selection.hosts >>= Apply.run flake apply
        else logAttention_ "No changes"
