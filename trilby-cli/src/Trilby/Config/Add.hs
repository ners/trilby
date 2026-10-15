module Trilby.Config.Add where

import Data.Map.Strict qualified as Map
import Trilby.Config.Apply qualified as Apply
import Trilby.Config.Cache (Cache)
import Trilby.Config.Edit (Element (..), Location (..))
import Trilby.Config.Edit qualified as Edit
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory, Kind (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.File qualified as File
import Trilby.Config.Options (ChangeOpts (..), Selection (..), Visible (..))
import Trilby.Config.Origin (Origin (..))
import Trilby.Config.Profile qualified as Profile
import Trilby.Config.Rows (Candidate (..))
import Trilby.Config.Rows qualified as Rows
import Trilby.Config.Table (Row (..))
import Trilby.Config.Target (Destination (..), Domain (..))
import Trilby.Config.Target qualified as Target
import Trilby.Prelude
import Trilby.Widgets (yesNoButtons)

warnProvided
    :: (Configuring es) => Cache -> Path Abs Dir -> Inventory -> Text -> Bool -> Text -> Eff es ()
warnProvided cache flake inventory host yes item = do
    user <- Target.currentUser
    listings <- Eval.listings Packages cache flake [host] [user]
    let provided =
            [ row
            | listing <- maybeToList $ Map.lookup host listings
            , candidate@Candidate{row} <- Rows.listingItems Packages inventory mempty Everything host listing
            , Rows.named item candidate
            ]
    unless (null provided) do
        for_ provided \Row{origin, file} ->
            logAttention_ $ item <> " is already provided by " <> ishow origin <> " " <> fromMaybe "" file
        unless yes . unlessM (yesNoButtons "Add it anyway?" False) $ liftIO exitSuccess

check
    :: (Configuring es) => Kind -> Cache -> Path Abs Dir -> Inventory -> Text -> Bool -> Text -> Eff es ()
check Packages cache flake inventory host yes item = do
    unlessM (Eval.packageExists flake host item) . errorExit $ "No such package: " <> item
    warnProvided cache flake inventory host yes item
check Services _ flake _ host _ item =
    unlessM (Eval.serviceExists flake host item) . errorExit $ "No such service: " <> item
check Profiles _ _ _ _ _ _ = pure ()

editFor
    :: (Configuring es)
    => Kind
    -> [Text]
    -> Path Abs Dir
    -> Inventory
    -> Text
    -> Text
    -> Destination
    -> Eff es (Text -> Either Edit.Error Text)
editFor Packages only _ _ _ item Destination{domain} =
    pure
        . Edit.addElement Location{attribute = Target.packagesAttribute domain, condition = only}
        $ Package item
editFor Services _ _ _ _ item _ = pure $ Edit.setValue [] ["services", item, "enable"] "true"
editFor Profiles _ flake inventory host item Destination{file, domain} = do
    nixpkgs <- Eval.nixpkgs flake host
    found <-
        maybe (errorExit $ "No such profile: " <> item) pure
            =<< Profile.find flake inventory nixpkgs item
    when (domain == Account && found.origin /= Local)
        . errorExit
        $ "Only local profiles can be added to users"
    element <- either errorExit pure $ Profile.element flake file found
    pure $ Edit.addElement Location{attribute = ["imports"], condition = []} element

add :: (Configuring es) => Kind -> ChangeOpts Maybe -> Eff es ()
add kind ChangeOpts{selection = selection@Selection{flake}, file, only, yes, apply, items} = do
    unless (null only) case kind of
        Packages -> pure ()
        _ -> errorExit "--only only applies to packages"
    inventory <- Eval.inventory flake
    hosts@(host : _) <- Target.hostNames inventory selection.hosts
    targets <- Target.destinations selection file inventory
    for_ items \item -> do
        check kind (Target.cacheOf selection inventory) flake inventory host yes item
        for_ targets \target -> do
            Target.createHome target
            edit <- editFor kind only flake inventory host item target
            File.modify target.file edit
    Apply.run flake apply hosts
