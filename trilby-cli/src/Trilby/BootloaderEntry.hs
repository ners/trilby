module Trilby.BootloaderEntry where

import Data.Aeson
    ( FromJSON (parseJSON)
    , Options (fieldLabelModifier)
    , defaultOptions
    , genericParseJSON
    , withText
    )
import Data.List.Extra qualified as List
import Trilby.Host (Host (..), ssh)
import Prelude

data BootloaderEntryType
    = Auto
    | Loader
    | Other Text
    | Type1
    deriving stock (Generic, Show, Eq)

instance FromJSON BootloaderEntryType where
    parseJSON = withText "BootloaderEntryType" \case
        "auto" -> pure Auto
        "loader" -> pure Loader
        "type1" -> pure Type1
        other -> pure $ Other other

data BootloaderEntry = BootloaderEntry
    { type' :: BootloaderEntryType
    , id :: Text
    , isDefault :: Bool
    , isSelected :: Bool
    }
    deriving stock (Generic, Show)

instance FromJSON BootloaderEntry where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \s -> fromMaybe s $ List.stripSuffix "'" s}

getBootloaderEntries :: Host -> App [BootloaderEntry]
getBootloaderEntries host = ssh host readProcessOutJson' ["bootctl", "list", "--json=short", "--no-pager"]
