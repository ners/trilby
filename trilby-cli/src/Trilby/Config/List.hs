module Trilby.Config.List where

import Data.List (nub)
import Data.List.Extra (nubSort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Entry (..), Kind (..), Listing (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Json qualified as Json
import Trilby.Config.Options (ListOpts (..), Selection (..), Visible (..))
import Trilby.Config.Rows (Candidate (..), fromOrigins, listingItems, ownedBy, title)
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Table (Row (..))
import Trilby.Config.Table qualified as Table
import Trilby.Config.Target qualified as Target
import Trilby.Prelude

outPaths :: Map Text Listing -> [Text]
outPaths listings =
    nubSort
        [ path
        | Listing{system, users} <- Map.elems listings
        , Entry{storePath = Just path} <- system <> mconcat (Map.elems users)
        ]

renderer :: Bool -> Bool -> Text -> [Row] -> [Text]
renderer json unique title
    | json && unique = Json.unique
    | json = Json.rows
    | unique = Table.unique title
    | otherwise = Table.render title

pathNamed :: Listing -> Listing
pathNamed Listing{..} = Listing{system = rename <$> system, users = fmap rename <$> users, ..}
  where
    rename Entry{name, ..} = Entry{name = fromMaybe name storePath, ..}

list :: (Configuring es) => Kind -> ListOpts -> Eff es ()
list kind ListOpts{selection = selection@Selection{flake}, visible, from, files, unique, json, size, fullPath} = do
    case kind of
        Packages -> pure ()
        _ -> when (size || fullPath) $ errorExit "--size and --full-path only apply to packages"
    inventory <- Eval.inventory flake
    let cache = Target.cacheOf selection inventory
    hosts <- Target.hostNames inventory selection.hosts
    users <- case (visible, selection.users) of
        (SystemOnly, _) -> pure []
        (_, Nothing) -> (: []) <$> Target.currentUser
        (_, Just selector) ->
            Selector.select selector . nub . mconcat . Map.elems <$> Eval.users cache flake hosts
    listings <-
        Eval.listingsWith
            (if size || fullPath then Eval.WithPaths else Eval.NamesOnly)
            kind
            cache
            flake
            hosts
            users
    sizes <- Eval.narSizes $ if size then outPaths listings else []
    let rows =
            [ row
            | (host, listing) <- Map.toList listings
            , Candidate{row} <-
                listingItems kind inventory sizes visible host $ if fullPath then pathNamed listing else listing
            , fromOrigins from row
            , maybe True (`ownedBy` row) selection.profile
            ]
                <&> \row -> if files then row else row{file = Nothing}
    liftIO . Text.putStr . Text.unlines $ renderer json unique (title kind) rows
