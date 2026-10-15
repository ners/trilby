module Trilby.Config.Remove where

import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Trilby.Config.Apply qualified as Apply
import Trilby.Config.Edit (Element (..), Location (..))
import Trilby.Config.Edit qualified as Edit
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Entry (..), Inventory (..), Kind (..), Listing (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.File qualified as File
import Trilby.Config.Options (ChangeOpts (..), Selection (..), Visible (..))
import Trilby.Config.Origin (Origin (..))
import Trilby.Config.Profile qualified as Profile
import Trilby.Config.Rows (Candidate (..))
import Trilby.Config.Rows qualified as Rows
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Table (Row (..), Scope (..))
import Trilby.Config.Target (Destination (..))
import Trilby.Config.Target qualified as Target
import Trilby.Prelude
import Trilby.Widgets (yesNoButtons)

definitions :: Kind -> Inventory -> Text -> Map Text Listing -> [Row]
definitions kind inventory item listings =
    [ row
    | (host, listing) <- Map.toList listings
    , candidate@Candidate{row} <- Rows.listingItems kind inventory mempty Everything host listing
    , Rows.named item candidate
    ]

describe :: [Row] -> Text
describe rows =
    Text.intercalate ", " $ nub [ishow origin <> " " <> fromMaybe "" file | Row{origin, file} <- rows]

confirmShared :: (Configuring es) => Bool -> Text -> [Row] -> Eff es ()
confirmShared yes item affected = unless yes do
    logAttention_
        $ "This removes "
        <> item
        <> " for all these hosts: "
        <> Text.unwords (nub [host | Row{scope = Scope{host}} <- affected])
        <> " and users: "
        <> Text.unwords (nub [user | Row{scope = Scope{user = Just user}} <- affected])
    unlessM (yesNoButtons "Is that what you want?" False) $ liftIO exitSuccess

removeDefined :: (Configuring es) => Kind -> ChangeOpts Maybe -> Eff es ()
removeDefined kind ChangeOpts{selection = selection@Selection{flake}, file, yes, apply, items} = do
    when (isJust file) $ errorExit "--file does not apply, the defining files are found automatically"
    inventory <- Eval.inventory flake
    selectedHosts <- Target.hostNames inventory selection.hosts
    let cache = Target.cacheOf selection inventory
    knownUsers <- nub . mconcat . Map.elems <$> Eval.users cache flake inventory.hosts
    selectedUsers <- case selection.users of
        Nothing -> (: []) <$> Target.currentUser
        Just selector -> pure $ Selector.select selector knownUsers
    listings <- Eval.listings kind cache flake inventory.hosts knownUsers
    let selected Row{scope = Scope{host, user}} = host `elem` selectedHosts && all (`elem` selectedUsers) user
    for_ items \item -> do
        let everywhere = definitions kind inventory item listings
        let inScope = filter selected everywhere
        when (null inScope) . errorExit $ item <> " is not defined for the selected hosts and users"
        let external = [row | row@Row{origin} <- inScope, origin /= Local]
        let localFiles = nub [file | Row{origin = Local, file = Just file} <- inScope]
        let affected = [row | row@Row{origin = Local, file = Just file} <- everywhere, file `elem` localFiles]
        case kind of
            Packages ->
                unless (null external) . errorExit $ item <> " is provided by " <> describe external
            _ -> pure ()
        unless (all selected affected) $ confirmShared yes item affected
        for_ localFiles \localFile -> do
            path <- Target.moduleFile flake localFile
            let edit = case kind of
                    Packages ->
                        Edit.removeElement
                            (Target.packagesAttribute $ Target.domainOf flake path)
                            (Edit.sameElement $ Package item)
                    _ -> Edit.removeBinding ["services", item, "enable"]
            File.tryModify path edit >>= \case
                Left Edit.NotFound -> errorExit $ "Cannot find " <> item <> " in " <> localFile
                Left failure -> errorExit $ File.describe path failure
                Right () -> pure ()
        for_ (nub [(host, owner) | Row{scope = Scope{host, user = owner}} <- external]) \(host, owner) -> do
            target <- maybe (Target.hostFile flake host) (Target.homeFile flake) owner
            File.modify target.file
                $ Edit.setValue ["lib"] ["services", item, "enable"] "lib.mkForce false"
    Apply.run flake apply selectedHosts

removeProfiles :: (Configuring es) => ChangeOpts Maybe -> Eff es ()
removeProfiles ChangeOpts{selection = selection@Selection{flake}, file, apply, items} = do
    inventory <- Eval.inventory flake
    hosts@(host : _) <- Target.hostNames inventory selection.hosts
    targets <- Target.destinations selection file inventory
    user <- Target.currentUser
    Listing{nixpkgs, system, users} <-
        maybe (errorExit $ "Cannot evaluate " <> host) pure
            . Map.lookup host
            =<< Eval.listings Profiles (Target.cacheOf selection inventory) flake [host] [user]
    let enabled =
            [ name
            | Entry{files} <- system <> mconcat (Map.elems users)
            , path <- files
            , Just (_, name) <- [Profile.nameOf (Eval.roots inventory nixpkgs) path]
            ]
    for_ items \item -> do
        unless (item `elem` enabled) . errorExit $ "Profile " <> item <> " is not enabled on " <> host
        for_ targets \target ->
            File.tryModify target.file (Edit.removeElement ["imports"] $ Profile.imports item) >>= \case
                Right () -> pure ()
                Left Edit.NotFound -> do
                    found <-
                        maybe (errorExit $ "No such profile: " <> item) pure
                            =<< Profile.find flake inventory nixpkgs item
                    element <- either errorExit pure $ Profile.element flake target.file found
                    File.modify target.file
                        $ Edit.addElement Location{attribute = ["disabledModules"], condition = []} element
                Left failure -> errorExit $ File.describe target.file failure
    Apply.run flake apply hosts

remove :: (Configuring es) => Kind -> ChangeOpts Maybe -> Eff es ()
remove Profiles = removeProfiles
remove kind = removeDefined kind
