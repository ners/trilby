module Trilby.Widgets where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Rope.Zipper qualified as RopeZipper
import System.Terminal.Widgets.Buttons
import System.Terminal.Widgets.Common (runWidgetIO)
import System.Terminal.Widgets.SearchSelect
import System.Terminal.Widgets.Select
import System.Terminal.Widgets.TextInput
import Prelude

textInputOpts :: (HasCallStack) => Bool -> Bool -> Text -> Text -> App Text
textInputOpts multiline required ((<> " ") -> prompt) (RopeZipper.fromText -> value) = do
    logDebug prompt
    text <- runWidgetIO TextInput{valueTransform = id, ..}
    pure $ RopeZipper.toText text.value

textInput :: Text -> Text -> App Text
textInput = textInputOpts False True

multilineTextInput :: Text -> Text -> App Text
multilineTextInput = textInputOpts True False

passwordInput :: Text -> App Text
passwordInput ((<> " ") -> prompt) = do
    let getPw :: TextInput -> App Text
        getPw = fmap (RopeZipper.toText . (.value)) . runWidgetIO
    let input =
            TextInput
                { prompt
                , value = ""
                , multiline = False
                , required = True
                , valueTransform = each .~ '*'
                }
    pw1 <- getPw input
    pw2 <- getPw $ input & #prompt .~ "Repeat password: "
    if pw1 == pw2
        then pure pw1
        else do
            logError "Passwords do not match"
            passwordInput prompt

buttons :: (Eq a, Show a) => Text -> [(a, Char)] -> Int -> (a -> Text) -> App a
buttons prompt values selected buttonText = do
    logDebug prompt
    b <-
        runWidgetIO
            Buttons
                { prompt
                , buttons = [(buttonText s, Just c) | (s, c) <- values]
                , selected
                }
    case values !? b.selected of
        Just (a, _) -> pure a
        Nothing -> do
            logError "Invalid selection"
            buttons prompt values selected buttonText

yesNoButtons :: Text -> Bool -> App Bool
yesNoButtons prompt defaultValue = do
    let values = [("Yes", 'Y'), ("No", 'N')] :: [(Text, Char)]
    let selected = if defaultValue then 0 else 1
    ("Yes" ==) <$> buttons prompt values selected id

multiSelect :: (Eq a, Show a) => Text -> [a] -> [a] -> (a -> Text) -> Int -> Int -> App [a]
multiSelect prompt options selections optionText minSelect maxSelect
    | minSelect > maxSelect = error "multiSelect: minSelect must be < maxSelect"
    | length options < minSelect = error "multiSelect: called with fewer options than minSelect"
    | otherwise =
        runWidgetIO
            Select
                { options =
                    [ SelectOption
                        { value
                        , checked = value `elem` selections
                        }
                    | value <- options
                    ]
                , cursorOption = 0
                , ..
                }
            <&> (fmap (.value) . filter (.checked) . (.options))

multiSelect1 :: (Eq a, Show a) => Text -> [a] -> [a] -> (a -> Text) -> Int -> Int -> App (NonEmpty a)
multiSelect1 prompt options selections optionText minSelect maxSelect
    | minSelect < 1 = error "multiSelect1: minSelect must be > 0"
    | otherwise = NonEmpty.fromList <$> multiSelect prompt options selections optionText minSelect maxSelect

multiSelectEnum :: (Eq a, Bounded a, Enum a, Show a) => Text -> [a] -> Int -> Int -> App [a]
multiSelectEnum prompt selections = multiSelect prompt [minBound .. maxBound] selections ishow

multiSelectEnum1 :: (Eq a, Bounded a, Enum a, Show a) => Text -> [a] -> Int -> Int -> App (NonEmpty a)
multiSelectEnum1 prompt selections = multiSelect1 prompt [minBound .. maxBound] selections ishow

select :: (Eq a, Show a) => Text -> [a] -> Maybe a -> (a -> Text) -> App a
select prompt options selection optionText
    | [o] <- options = pure o
    | null options, Just o <- selection = pure o
    | null options = errorExit "select: called with no options and no default value"
    | otherwise = NonEmpty.head <$> multiSelect1 prompt options (maybeToList selection) optionText 1 1

selectEnum :: (Eq a, Bounded a, Enum a, Show a) => Text -> Maybe a -> App a
selectEnum prompt defaultValues = select prompt [minBound .. maxBound] defaultValues ishow

searchSelect :: (Eq a, Show a) => Text -> [a] -> [a] -> (a -> Text) -> App [a]
searchSelect ((<> " ") -> prompt) options selections optionText
    | length options < 2 = pure selections
    | otherwise =
        runWidgetIO
            SearchSelect
                { prompt
                , searchValue = ""
                , options
                , visibleOptions = []
                , selections
                , optionText
                , newOption = const Nothing
                , minSelect = 1
                , maxSelect = 1
                , maxVisible = 5
                , minSearchLength = 3
                , cursorRow = 0
                }
            <&> (.selections)
