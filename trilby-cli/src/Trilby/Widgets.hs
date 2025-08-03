module Trilby.Widgets where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Rope.Zipper qualified as RopeZipper
import System.Terminal.Widgets.Buttons
import System.Terminal.Widgets.Common (runWidgetIO)
import System.Terminal.Widgets.SearchSelect
import System.Terminal.Widgets.Select
import System.Terminal.Widgets.TextInput
import Trilby.Prelude

textInputOpts :: Bool -> Bool -> Text -> Text -> App Text
textInputOpts multiline required ((<> " ") -> prompt) (RopeZipper.fromText -> value) = do
    text <- runWidgetIO TextInput{valueTransform = id, ..}
    pure $ RopeZipper.toText text.value

textInput :: Text -> Text -> App Text
textInput = textInputOpts False True

multilineTextInput :: Text -> Text -> App Text
multilineTextInput = textInputOpts True False

passwordInput :: (HasCallStack) => Text -> App Text
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
            logAttention_ "Passwords do not match"
            passwordInput prompt

buttons :: (HasCallStack, Eq a, Show a) => Text -> [(a, Char)] -> Int -> (a -> Text) -> App a
buttons prompt values selected buttonText = do
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
            logAttention_ "Invalid selection"
            buttons prompt values selected buttonText

yesNoButtons :: (HasCallStack) => Text -> Bool -> App Bool
yesNoButtons prompt defaultValue = do
    let values = [("Yes", 'Y'), ("No", 'N')] :: [(Text, Char)]
    let selected = if defaultValue then 0 else 1
    ("Yes" ==) <$> buttons prompt values selected id

multiSelect :: (HasCallStack, Eq a, Show a) => Text -> [a] -> [a] -> (a -> Text) -> Int -> Int -> App [a]
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

multiSelect1 :: (HasCallStack, Eq a, Show a) => Text -> [a] -> [a] -> (a -> Text) -> Int -> Int -> App (NonEmpty a)
multiSelect1 prompt options selections optionText minSelect maxSelect
    | minSelect < 1 = errorExit "multiSelect1: minSelect must be > 0"
    | otherwise = NonEmpty.fromList <$> multiSelect prompt options selections optionText minSelect maxSelect

multiSelectEnum :: (HasCallStack, Eq a, Bounded a, Enum a, Show a) => Text -> [a] -> Int -> Int -> App [a]
multiSelectEnum prompt selections = multiSelect prompt [minBound .. maxBound] selections ishow

multiSelectEnum1 :: (HasCallStack, Eq a, Bounded a, Enum a, Show a) => Text -> [a] -> Int -> Int -> App (NonEmpty a)
multiSelectEnum1 prompt selections = multiSelect1 prompt [minBound .. maxBound] selections ishow

select :: (HasCallStack, Eq a, Show a) => Text -> [a] -> Maybe a -> (a -> Text) -> App a
select prompt options selection optionText
    | [o] <- options = pure o
    | null options, Just o <- selection = pure o
    | null options = errorExit "select: called with no options and no default value"
    | otherwise = NonEmpty.head <$> multiSelect1 prompt options (maybeToList selection) optionText 1 1

selectEnum :: (HasCallStack, Eq a, Bounded a, Enum a, Show a) => Text -> Maybe a -> App a
selectEnum prompt defaultValues = select prompt [minBound .. maxBound] defaultValues ishow

searchSelect :: (HasCallStack, Eq a, Show a) => Text -> [a] -> [a] -> [a] -> (a -> Text) -> Int -> Int -> App [a]
searchSelect prompt options visibleOptions selections optionText minSelect maxSelect
    | minSelect > maxSelect = error "searchSelect: minSelect must be < maxSelect"
    | length options < minSelect = errorExit "searchSelect: need at least as many options as minSelect"
    | length options == minSelect = pure options
    | otherwise =
        runWidgetIO
            SearchSelect
                { prompt
                , searchValue = ""
                , options
                , visibleOptions
                , selections
                , optionText
                , newOption = const Nothing
                , minSelect
                , maxSelect
                , maxVisible = 10
                , minSearchLength = 2
                , cursorRow = 0
                }
            <&> (.selections)

searchSelectSome :: (HasCallStack, Eq a, Show a) => Text -> [a] -> [a] -> [a] -> (a -> Text) -> Int -> Int -> App (NonEmpty a)
searchSelectSome ((<> " ") -> prompt) options visibleOptions selections optionText minSelect maxSelect
    | minSelect < 1 = errorExit "searchSelect1: minSelect must be > 0"
    | otherwise = NonEmpty.fromList <$> searchSelect prompt options visibleOptions selections optionText minSelect maxSelect

searchSelect1 :: (HasCallStack, Eq a, Show a) => Text -> [a] -> [a] -> [a] -> (a -> Text) -> App a
searchSelect1 ((<> " ") -> prompt) options visibleOptions selections optionText =
    NonEmpty.head <$> searchSelectSome prompt options visibleOptions selections optionText 1 1
