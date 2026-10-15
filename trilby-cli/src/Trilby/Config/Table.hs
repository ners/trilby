module Trilby.Config.Table where

import Data.List (transpose)
import Data.List.Extra (nubSort, nubSortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Trilby.Config.Origin (Origin)
import Trilby.Config.Size qualified as Size
import Trilby.Prelude

data Scope = Scope
    { host :: Text
    , user :: Maybe Text
    }
    deriving stock (Eq, Ord)

data Row = Row
    { name :: Text
    , scope :: Scope
    , origin :: Origin
    , file :: Maybe Text
    , size :: Maybe Integer
    }
    deriving stock (Eq, Ord)

sortKey :: Row -> (Text, Text)
sortKey Row{name} = (Text.toLower . fromMaybe name $ dropStoreHash name, name)
  where
    dropStoreHash = fmap (Text.drop 1 . Text.dropWhile (/= '-')) . Text.stripPrefix "/nix/store/"

normalised :: [Row] -> [Row]
normalised = nubSortOn \row -> (sortKey row, row)

grouped :: [Row] -> [[Row]]
grouped rows = Map.elems $ Map.fromListWith (<>) [(sortKey row, [row]) | row <- rows]

largest :: [Row] -> Maybe Integer
largest group = maximum <$> nonEmpty (catMaybes [size | Row{size} <- group])

label :: Bool -> Scope -> Text
label _ Scope{host, user = Nothing} = "host:" <> host
label qualified Scope{host, user = Just user}
    | qualified = "user:" <> user <> "@" <> host
    | otherwise = "user:" <> user

tabulate :: [Text] -> [[Text]] -> [Text]
tabulate header body = line <$> table
  where
    table = header : body
    line = Text.stripEnd . Text.intercalate "  " . zipWith (`Text.justifyLeft` ' ') widths
    widths = maximum . fmap Text.length <$> transpose table

hostQualified :: [Row] -> Bool
hostQualified rows = Set.size (Set.fromList [host | Row{scope = Scope{host}} <- rows]) > 1

hasFiles, hasSizes :: [Row] -> Bool
hasFiles = any (isJust . (.file))
hasSizes = any (isJust . (.size))

headerOf :: Text -> [Row] -> [Text]
headerOf title rows =
    [title] <> ["SIZE" | hasSizes rows] <> ["SCOPE", "FROM"] <> ["FILE" | hasFiles rows]

sizeCell :: Maybe Integer -> Text
sizeCell = maybe "" Size.format

commaSeparated :: [Text] -> Text
commaSeparated = Text.intercalate ","

columns :: Text -> [Row] -> [[Row]] -> [Text]
columns _ [] _ = ["(none)"]
columns title rows groups = tabulate (headerOf title rows) (cells <$> groups)
  where
    qualified = hostQualified rows
    cells group@(Row{name} : _) =
        [name]
            <> [sizeCell $ largest group | hasSizes rows]
            <> [ commaSeparated $ label qualified <$> nubSort [scope | Row{scope} <- group]
               , commaSeparated $ ishow <$> nubSort [origin | Row{origin} <- group]
               ]
            <> [commaSeparated . nubSort $ catMaybes [file | Row{file} <- group] | hasFiles rows]
    cells [] = []

render :: Text -> [Row] -> [Text]
render title rows = columns title rows $ pure <$> normalised rows

unique :: Text -> [Row] -> [Text]
unique title rows = columns title rows $ grouped rows
