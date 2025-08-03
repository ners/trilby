module Trilby.Install.Config.Keyboard where

import Data.ByteString qualified as ByteString
import Data.Either (fromRight)
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Text qualified as Text
import Effectful.Fail (runFail)
import Text.XML.Light qualified as XML
import Text.XML.Light.Cursor qualified as XML (Cursor)
import Text.XML.Light.Cursor qualified as XML.Cursor
import Trilby.HNix
import Prelude

data Keyboard = Keyboard
    { layout :: Text
    , variant :: Maybe Text
    , description :: Maybe Text
    }
    deriving stock (Generic)

instance Show Keyboard where
    show Keyboard{..} = fromText . mconcat $ [layout, variantStr, descriptionStr]
      where
        variantStr
            | Just variant <- variant = "(" <> variant <> ")"
            | otherwise = ""
        descriptionStr
            | Just description <- description = ": " <> description
            | otherwise = ""

instance Eq Keyboard where
    a == b = a.layout == b.layout && a.variant == b.variant

instance Ord Keyboard where
    compare = comparing (.layout) <> comparing (.variant)

instance ToExpr Keyboard where
    toExpr Keyboard{..} =
        [nix|
        {
            layout = layout;
            variant = variant;
        }
        |]
            & canonicalSet

getCurrentKeyboard :: (HasCallStack) => App (Maybe Keyboard)
getCurrentKeyboard = do
    localeStatus <- cmdOutTextLines ["localectl", "status", "--full"]
    let layout = fmap Text.strip . listToMaybe $ mapMaybe (Text.stripPrefix "X11 Layout:") localeStatus
        variant = fmap Text.strip . listToMaybe $ mapMaybe (Text.stripPrefix "X11 Variant:") localeStatus
    forM layout \layout -> pure $ Keyboard{layout, variant, description = Nothing}

getAllKeyboards :: (HasCallStack) => App [Keyboard]
getAllKeyboards =
    fromRight [] <$> runFail do
        Just cursor <- liftIO $ XML.Cursor.fromForest . XML.parseXML <$> ByteString.readFile "/usr/share/X11/xkb/rules/base.xml"
        Just layoutList <- pure $ cursorToElement =<< XML.Cursor.findRec ((Just "layoutList" ==) . fmap (XML.qName . XML.elName) . cursorToElement) cursor
        let layouts = findChildrenByName "layout" layoutList
        pure . List.sort . flip concatMap layouts $ \layout ->
            let (baseName, baseDescription) = fromMaybe (error $ "Could not find name and description in " <> show layout) $ nameAndDescription layout
                baseKeyboard = Keyboard{layout = baseName, variant = Nothing, description = Just baseDescription}
                variants = maybe [] (findChildrenByName "variant") $ findChildByName "variantList" layout
             in baseKeyboard
                    : [ Keyboard{layout = baseName, variant = Just name, description = Just description}
                      | Just (name, description) <- nameAndDescription <$> variants
                      ]
  where
    findChildByName :: String -> XML.Element -> Maybe XML.Element
    findChildByName qName = XML.findChild XML.blank_name{XML.qName}

    findChildrenByName :: String -> XML.Element -> [XML.Element]
    findChildrenByName qName = XML.findChildren XML.blank_name{XML.qName}

    cursorToElement :: XML.Cursor -> Maybe XML.Element
    cursorToElement =
        XML.Cursor.current >>> \case
            XML.Elem e -> Just e
            _ -> Nothing

    nameAndDescription :: XML.Element -> Maybe (Text, Text)
    nameAndDescription e = do
        configItem <- findChildByName "configItem" e
        name <- XML.strContent <$> findChildByName "name" configItem
        description <- XML.strContent <$> findChildByName "description" configItem
        pure (fromString name, fromString description)
