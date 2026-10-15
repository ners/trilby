module Trilby.Config.Search where

import Data.List (intercalate, nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Kind)
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Json qualified as Json
import Trilby.Config.Options (SearchOpts (..), Selection (..), Visible (..))
import Trilby.Config.Rows (Candidate (..))
import Trilby.Config.Rows qualified as Rows
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Table (Row (..))
import Trilby.Config.Table qualified as Table
import Trilby.Config.Target qualified as Target
import Trilby.Prelude

matches :: Bool -> Text -> Candidate -> Bool
matches exact query candidate@Candidate{row = Row{name}}
    | exact = Rows.named query candidate
    | otherwise = Text.toLower query `Text.isInfixOf` Text.toLower name

sections :: [(Kind, [Row])] -> [Text]
sections results = case filter (not . null . snd) results of
    [] -> ["(none)"]
    found -> intercalate [""] [Table.unique (Rows.title kind) rows | (kind, rows) <- found]

search :: (Configuring es) => SearchOpts -> Eff es ()
search SearchOpts{selection = selection@Selection{flake}, ..} = do
    inventory <- Eval.inventory flake
    let cache = Target.cacheOf selection inventory
    hosts <- Target.hostNames inventory . Just $ fromMaybe "*" selection.hosts
    users <- case visible of
        SystemOnly -> pure []
        _ ->
            Selector.select (fromMaybe "*" selection.users)
                . nub
                . mconcat
                . Map.elems
                <$> Eval.users cache flake hosts
    results <- for kinds \kind -> do
        listings <- Eval.listings kind cache flake hosts users
        pure
            ( kind
            , [ if files then item.row else item.row{file = Nothing}
              | (host, listing) <- Map.toList listings
              , item <- Rows.listingItems kind inventory mempty visible host listing
              , matches exact query item
              , Rows.fromOrigins from item.row
              , maybe True (`Rows.ownedBy` item.row) selection.profile
              ]
            )
    liftIO
        . Text.putStr
        . Text.unlines
        $ if json
            then Json.search [(Eval.kindName kind, rows) | (kind, rows) <- results]
            else sections results
