module Trilby.Config.Users where

import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Effectful.FileSystem.Path.IO (doesFileExist)
import Trilby.Config.Apply qualified as Apply
import Trilby.Config.Edit (Element (..), Location (..))
import Trilby.Config.Edit qualified as Edit
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.File qualified as File
import Trilby.Config.Options (ChangeOpts (..), ListOpts (..), Selection (..))
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Target (Destination (..))
import Trilby.Config.Target qualified as Target
import Trilby.HNix (writeNixFile)
import Trilby.Install (hashedPassword)
import Trilby.Install.Config.User (User (..))
import Trilby.Install.Options (validateUsername)
import Trilby.Prelude
import Trilby.Widgets (passwordInput)

imports :: Location
imports = Location{attribute = ["imports"], condition = []}

matches :: Text -> Text -> Bool
matches user =
    Edit.hasSuffix
        ["users." <> user, "users." <> user <> "{}", "users/" <> user, "users/" <> user <> "/default.nix"]

userElement :: [Text] -> Text -> Text
userElement siblings user =
    fromMaybe ("../../users/" <> user) . asum @[] $ retarget <$> siblings <*> ["users.", "users/"]
  where
    retarget element marker = do
        let (before, after) = Text.breakOn marker element
        guard . not $ Text.null after
        let rest = Text.drop (Text.length marker) after
        let name = Text.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') rest
        guard . not $ Text.null name
        pure $ before <> marker <> user <> Text.drop (Text.length name) rest

hostConfiguration :: Text -> Text
hostConfiguration host = "hosts/" <> host <> "/configuration.nix"

importsIn :: (Configuring es) => Path Abs File -> Eff es [Text]
importsIn file =
    ifM
        (doesFileExist file)
        (fromRight [] . Edit.elementsOf ["imports"] <$> liftIO (Text.readFile $ fromPath file))
        (pure [])

targets :: (Configuring es) => Selection -> Maybe FilePath -> [Text] -> Eff es [Destination]
targets Selection{flake, profile, users} file hosts
    | Just path <- file = (: []) <$> Target.explicitFile flake path
    | isJust profile || isJust users = errorExit "Users belong to hosts, use --host"
    | otherwise = traverse (Target.hostFile flake) hosts

create :: (Configuring es) => Path Abs Dir -> Text -> Eff es ()
create flake user = do
    known <- Target.localUsers flake
    unless (user `elem` known) do
        either (errorExit . fromString) (const $ pure ()) $ validateUsername user
        password <- hashedPassword =<< passwordInput ("Choose password for " <> user <> ":")
        file <- Target.relativeFile flake $ "users/" <> user <> "/default.nix"
        writeNixFile file User{uid = 1000 + length known, username = user, password = Just password}
        logAttention_ $ "Created " <> fromPath file <> ", make sure that Nix can see it"

list :: (Configuring es) => ListOpts -> Eff es ()
list ListOpts{selection = selection@Selection{flake}} = do
    inventory <- Eval.inventory flake
    hosts <- Target.hostNames inventory selection.hosts
    users <- Eval.users (Target.cacheOf selection inventory) flake hosts
    for_ hosts \host -> do
        liftIO $ Text.putStrLn host
        let names = maybe id Selector.select selection.users $ Map.findWithDefault [] host users
        liftIO . Text.putStr . Text.unlines $ case names of
            [] -> ["  (none)"]
            _ -> ("  " <>) <$> names

add :: (Configuring es) => ChangeOpts Maybe -> Eff es ()
add ChangeOpts{selection = selection@Selection{flake}, file, only, apply, items} = do
    unless (null only) $ errorExit "--only only applies to packages"
    inventory <- Eval.inventory flake
    hosts <- Target.hostNames inventory selection.hosts
    destinations <- targets selection file hosts
    for_ items \user -> do
        create flake user
        others <- traverse (importsIn <=< Target.relativeFile flake . hostConfiguration) inventory.hosts
        for_ destinations \Destination{file} -> do
            own <- importsIn file
            File.modify file
                . Edit.addElement imports
                $ Verbatim{arguments = [], text = userElement (own <> mconcat others) user}
    Apply.run flake apply hosts

remove :: (Configuring es) => ChangeOpts Maybe -> Eff es ()
remove ChangeOpts{selection = selection@Selection{flake}, file, apply, items} = do
    inventory <- Eval.inventory flake
    hosts <- Target.hostNames inventory selection.hosts
    destinations <- targets selection file hosts
    for_ items \user -> for_ destinations \Destination{file} ->
        File.tryModify file (Edit.removeElement ["imports"] $ matches user) >>= \case
            Left Edit.NotFound -> errorExit $ user <> " is not imported in " <> fromPath file
            Left failure -> errorExit $ File.describe file failure
            Right () -> pure ()
    Apply.run flake apply hosts
