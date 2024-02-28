module Trilby.Widgets where

import Data.Text.Rope.Zipper qualified as RopeZipper
import System.Terminal
import System.Terminal.Widgets.Buttons
import System.Terminal.Widgets.Common qualified as Terminal
import System.Terminal.Widgets.SearchSelect
import System.Terminal.Widgets.Select
import System.Terminal.Widgets.TextInput
import Prelude

runWidget :: (Terminal.Widget w) => w -> App w
runWidget = liftIO . withTerminal . runTerminalT . Terminal.runWidget

textInput :: Text -> Text -> App Text
textInput ((<> " ") -> prompt) (RopeZipper.fromText -> value) = do
    $(logDebug) prompt
    text <-
        runWidget
            TextInput
                { prompt
                , value
                , multiline = False
                , required = True
                , valueTransform = id
                }
    pure $ RopeZipper.toText text.value

passwordInput :: Text -> App Text
passwordInput ((<> " ") -> prompt) = do
    $(logDebug) prompt
    let getPw :: TextInput -> App Text
        getPw = fmap (RopeZipper.toText . (.value)) . runWidget
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
            $(logError) "Passwords do not match"
            passwordInput prompt

buttons :: (Eq a, Show a) => Text -> [(a, Char)] -> Int -> (a -> Text) -> App a
buttons prompt values selected buttonText = do
    $(logDebug) prompt
    b <-
        runWidget
            Buttons
                { prompt
                , buttons = [(buttonText s, Just c) | (s, c) <- values]
                , selected
                }
    case values !? b.selected of
        Just (a, _) -> pure a
        Nothing -> do
            $(logError) "Invalid selection"
            buttons prompt values selected buttonText

yesNoButtons :: Text -> Bool -> App Bool
yesNoButtons prompt defaultValue = do
    $(logDebug) prompt
    let values = [("Yes", 'Y'), ("No", 'N')] :: [(Text, Char)]
    let selected = if defaultValue then 0 else 1
    ("Yes" ==) <$> buttons prompt values selected id

select :: (Eq a, Show a) => Text -> [a] -> Maybe a -> (a -> Text) -> App a
select prompt values defaultValue optionText
    | [value] <- values = pure value
    | null values, Just value <- defaultValue = pure value
    | null values = errorExit "select: called with no options and no default value"
    | otherwise = do
        selectedValues <-
            runWidget
                Select
                    { prompt
                    , options =
                        [ SelectOption
                            { value
                            , checked = Just value == defaultValue
                            }
                        | value <- values
                        ]
                    , optionText
                    , minSelect = 1
                    , maxSelect = 1
                    , cursorRow = 0
                    }
                <&> (fmap (.value) . filter (.checked) . (.options))
        case selectedValues of
            [value] -> pure value
            _ -> do
                $(logError) "Invalid selection"
                select prompt values defaultValue optionText

selectEnum :: (Eq a, Bounded a, Enum a, Show a) => Text -> Maybe a -> App a
selectEnum prompt defaultValues = select prompt [minBound .. maxBound] defaultValues ishow

searchSelect :: (Eq a, Show a) => Text -> [a] -> [a] -> (a -> Text) -> App [a]
searchSelect ((<> " ") -> prompt) values defaultValues optionText
    | length values < 2 = pure defaultValues
    | otherwise =
        runWidget
            SearchSelect
                { prompt
                , searchValue = ""
                , options = [SearchSelectOption{value, visible = False} | value <- values]
                , selections = defaultValues
                , optionText
                , minSelect = 1
                , maxSelect = 1
                , maxVisible = 5
                , minSearchLength = 3
                , cursorRow = 0
                }
            <&> (.selections)
