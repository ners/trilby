module Trilby.Install.Config.Keyboard where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Text qualified as Text
import Effectful.Fail (runFail)
import Effectful.FileSystem.Path.IO (doesFileExist)
import Text.XML.Light qualified as XML
import Text.XML.Light.Cursor qualified as XML (Cursor)
import Text.XML.Light.Cursor qualified as XML.Cursor
import Trilby.HNix
import Trilby.Prelude

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
getAllKeyboards = do
    eitherM (\e -> logAttention_ (fromString e) >> pure []) pure $ runFail do
        baseFile <- inject readBaseFile
        cursor <- maybe (fail "could not parse base.xml file") pure . XML.Cursor.fromForest . XML.parseXML $ baseFile
        layoutList <- maybe (fail "could not find layoutList") pure $ cursorToElement =<< XML.Cursor.findRec ((Just "layoutList" ==) . fmap (XML.qName . XML.elName) . cursorToElement) cursor
        let layouts = findChildrenByName "layout" layoutList
        fmap List.sort . flip concatMapM layouts $ \layout -> eitherM (\e -> logAttention_ (fromString e) >> pure []) pure $ runFail do
            (baseName, baseDescription) <- fromMaybeM (fail $ "Could not find name and description in " <> show layout) . pure . nameAndDescription $ layout
            pure $ Keyboard{layout = baseName, variant = Nothing, description = Just baseDescription}
                : [ Keyboard{layout = baseName, variant = Just name, description = Just description}
                  | Just (name, description) <- nameAndDescription <$> maybe [] (findChildrenByName "variant") (findChildByName "variantList" layout)
                  ]
  where
    baseFile :: Path Rel File
    baseFile = $(mkRelFile "share/X11/xkb/rules/base.xml")

    usrBaseFile :: Path Abs File
    usrBaseFile = $(mkAbsDir "/usr") </> baseFile

    nixBaseFile :: App [Path Abs File]
    nixBaseFile = fmap (fmap (</> baseFile)) . nixBuild . Flake =<< trilbyFlake ["xkeyboard-config"]

    runCurrentSystemBaseFile :: Path Abs File
    runCurrentSystemBaseFile = $(mkAbsDir "/run/current-system/sw") </> baseFile

    findBaseFile :: App (Path Abs File)
    findBaseFile =
        fromMaybeM (errorExit "could not find base.xml")
            . runMaybeT
            . foldr1 @[] (<|>)
            . fmap MaybeT
            $ [ findM doesFileExist [runCurrentSystemBaseFile, usrBaseFile]
              , findM doesFileExist =<< nixBaseFile
              ]

    readBaseFile :: App ByteString
    readBaseFile = do
        baseFile <- findBaseFile
        logTrace_ $ "reading base file: " <> fromPath baseFile
        liftIO . ByteString.readFile . fromPath $ baseFile

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
