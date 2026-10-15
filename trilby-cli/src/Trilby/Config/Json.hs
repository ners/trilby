module Trilby.Config.Json where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.Extra (nubSort)
import Data.Text.Encoding qualified as Text
import Trilby.Config.Origin (Origin)
import Trilby.Config.Table (Row (..), Scope (..))
import Trilby.Config.Table qualified as Table
import Trilby.Prelude hiding ((.=))

scopeValue :: Scope -> Value
scopeValue Scope{host, user} = object ["host" .= host, "user" .= user]

encoded :: [Value] -> [Text]
encoded values = [Text.decodeUtf8 . LazyByteString.toStrict $ Aeson.encode values]

rows :: [Row] -> [Text]
rows (Table.normalised -> sorted) = encoded $ value <$> sorted
  where
    value Row{..} =
        object
            $ ["name" .= name, "scope" .= scopeValue scope, "origin" .= ishow @Origin @Text origin]
            <> catMaybes [("file" .=) <$> file, ("size" .=) <$> size]

uniqueValues :: [Row] -> [Value]
uniqueValues rows = value <$> Table.grouped rows
  where
    value group =
        object
            $ [ "name" .= foldMap (.name) (listToMaybe group)
              , "scopes" .= (scopeValue <$> nubSort [scope | Row{scope} <- group])
              , "origins" .= (ishow @Origin @Text <$> nubSort [origin | Row{origin} <- group])
              ]
            <> catMaybes
                [ ("files" .=) <$> nonEmpty (nubSort $ catMaybes [file | Row{file} <- group])
                , ("size" .=) <$> Table.largest group
                ]

unique :: [Row] -> [Text]
unique = encoded . uniqueValues

withKind :: Text -> Value -> Value
withKind kind (Aeson.Object fields) = Aeson.Object $ KeyMap.insert "kind" (Aeson.toJSON kind) fields
withKind _ other = other

search :: [(Text, [Row])] -> [Text]
search results = encoded [withKind kind value | (kind, rows) <- results, value <- uniqueValues rows]
