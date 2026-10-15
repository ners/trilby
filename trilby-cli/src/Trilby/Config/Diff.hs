module Trilby.Config.Diff where

import Data.Aeson (Value, object, (.=))
import Data.List (intercalate, nub, sortOn)
import Data.List.Extra (nubSort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Cache (Cache (..))
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory (..), Kind (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Json qualified as Json
import Trilby.Config.Options (DiffOpts (..), Visible (..))
import Trilby.Config.Rows (Candidate (..))
import Trilby.Config.Rows qualified as Rows
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Table (Row (..), Scope (..))
import Trilby.Config.Table qualified as Table
import Trilby.Prelude hiding ((.=))

data Change = Change
    { kind :: Kind
    , user :: Maybe Text
    , name :: Text
    , left :: [Text]
    , right :: [Text]
    }

namesByKey :: [Candidate] -> Map (Maybe Text, Text) [Text]
namesByKey items =
    nubSort
        <$> Map.fromListWith (<>) [((row.scope.user, key), [row.name]) | Candidate{key, row} <- items]

changes :: Kind -> [Candidate] -> [Candidate] -> [Change]
changes kind leftItems rightItems =
    sortOn
        (\change -> (Text.toLower change.name, change.user))
        [ Change{..}
        | (user, name) <- Set.toAscList . Set.fromList $ Map.keys leftNames <> Map.keys rightNames
        , let left = Map.findWithDefault [] (user, name) leftNames
        , let right = Map.findWithDefault [] (user, name) rightNames
        , left /= right
        ]
  where
    leftNames = namesByKey leftItems
    rightNames = namesByKey rightItems

render :: Text -> Text -> Text -> [Change] -> [Text]
render leftHost rightHost title changes = Table.tabulate header (cells <$> changes)
  where
    scoped = any (isJust . (.user)) changes
    header = [title] <> ["USER" | scoped] <> [leftHost, rightHost]
    side names = if null names then "-" else Table.commaSeparated names
    cells Change{..} = [name] <> [fromMaybe "" user | scoped] <> [side left, side right]

value :: Change -> Value
value Change{..} =
    object
        [ "kind" .= Eval.kindName kind
        , "name" .= name
        , "user" .= user
        , "left" .= left
        , "right" .= right
        ]

sections :: Text -> Text -> [(Kind, [Change])] -> [Text]
sections leftHost rightHost results = case filter (not . null . snd) results of
    [] -> ["No differences"]
    found -> intercalate [""] [render leftHost rightHost (Rows.title kind) cs | (kind, cs) <- found]

diff :: (Configuring es) => DiffOpts -> Eff es ()
diff DiffOpts{..} = do
    when (left == right) $ errorExit "Choose two different hosts"
    inventory <- Eval.inventory flake
    for_ @[] [left, right] \host ->
        unless (host `elem` inventory.hosts)
            . errorExit
            $ "No such host: "
            <> host
            <> ", available hosts: "
            <> Text.unwords inventory.hosts
    let cache = Cache{refresh, self = inventory.self}
    selectedUsers <- case users of
        Nothing -> pure []
        Just selector ->
            Selector.select selector . nub . mconcat . Map.elems <$> Eval.users cache flake [left, right]
    let visible = if null selectedUsers then SystemOnly else Everything
    results <- for kinds \kind -> do
        listings <- Eval.listings kind cache flake [left, right] selectedUsers
        for_ @[] [left, right] \host ->
            unless (Map.member host listings) . errorExit $ "Cannot evaluate host " <> host
        let itemsOf host =
                [ item
                | listing <- maybeToList $ Map.lookup host listings
                , item <- Rows.listingItems kind inventory mempty visible host listing
                , Rows.fromOrigins from item.row
                ]
        pure (kind, changes kind (itemsOf left) (itemsOf right))
    liftIO
        . Text.putStr
        . Text.unlines
        $ if json
            then Json.encoded $ value <$> concatMap snd results
            else sections left right results
