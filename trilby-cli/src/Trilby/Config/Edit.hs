{-# LANGUAGE PatternSynonyms #-}

module Trilby.Config.Edit where

import Data.Bifunctor qualified as Bifunctor
import Data.Char (isSpace)
import Data.Either.Extra (maybeToEither)
import Data.List (sortOn, stripPrefix)
import Data.Text qualified as Text
import Nix.Expr.Types.Annotated (NExprLoc, SrcSpan (..), pattern Ann)
import Nix.Parser (parseNixTextLoc)
import Trilby.Prelude

data Error
    = NotFound
    | Failed String
    deriving stock (Eq, Show)

data Element
    = Verbatim {arguments :: [Text], text :: Text}
    | Package Text

data Location = Location
    { attribute :: [Text]
    , condition :: [Text]
    }

data Source = Source
    { text :: Text
    , lineStarts :: [Int]
    , root :: NExprLoc
    }

data Splice = Splice
    { from :: Int
    , to :: Int
    , replacement :: Text
    }

data Located = Located
    { position :: NSourcePos
    , value :: NExprLoc
    }

data Block = Block
    { operand :: NExprLoc
    , previousEnd :: Maybe Int
    , condition :: Maybe NExprLoc
    , list :: NExprLoc
    , elements :: [NExprLoc]
    }

parse :: Text -> Either Error Source
parse text
    | Text.elem '\t' text = Left $ Failed "tab characters are not supported"
    | otherwise = Bifunctor.bimap (Failed . show) (Source text lineStarts) $ parseNixTextLoc text
  where
    lineStarts = scanl (\offset line -> offset + Text.length line + 1) 0 $ Text.splitOn "\n" text

coordinates :: NSourcePos -> (Int, Int)
coordinates (NSourcePos _ (NPos line) (NPos column)) = (unPos line, unPos column)

offset :: Source -> NSourcePos -> Int
offset Source{lineStarts} (coordinates -> (line, column)) =
    fromMaybe 0 (lineStarts !? (line - 1)) + column - 1

bounds :: Source -> NExprLoc -> (Int, Int)
bounds src (Ann (SrcSpan begin end) _) = (offset src begin, offset src end)

beginsAt, endsAt :: NExprLoc -> (Int, Int)
beginsAt (Ann (SrcSpan begin _) _) = coordinates begin
endsAt (Ann (SrcSpan _ end) _) = coordinates end

slice :: Source -> NExprLoc -> Text
slice src@Source{text} expr = text & Text.drop from & Text.take (to - from)
  where
    (from, to) = bounds src expr

splice :: [Splice] -> Text -> Text
splice edits text = foldl' apply text $ sortOn (negate . (.from)) edits
  where
    apply current Splice{..} = Text.take from current <> replacement <> Text.drop to current

moduleSet :: NExprLoc -> Maybe NExprLoc
moduleSet expr@(Ann _ body) = case body of
    NSet{} -> Just expr
    NAbs _ inner -> moduleSet inner
    NLet _ inner -> moduleSet inner
    NWith _ inner -> moduleSet inner
    NApp _ argument -> moduleSet argument
    NBinary NUpdate left right -> moduleSet left <|> moduleSet right
    _ -> Nothing

moduleOf :: Source -> Either Error NExprLoc
moduleOf src = maybeToEither (Failed "unsupported module structure") $ moduleSet src.root

innermostAbs :: NExprLoc -> Maybe NExprLoc
innermostAbs expr@(Ann _ (NAbs _ body)) = innermostAbs body <|> Just expr
innermostAbs _ = Nothing

staticKeys :: NAttrPath r -> Maybe [Text]
staticKeys path = for (toList path) \case
    StaticKey (VarName name) -> Just name
    _ -> Nothing

findBinding :: [Text] -> NExprLoc -> Maybe Located
findBinding target (Ann _ (NSet _ bindings)) = asum $ look <$> bindings
  where
    look (NamedVar path value position) = do
        keys <- staticKeys path
        if keys == target
            then Just Located{..}
            else stripPrefix keys target >>= (`findBinding` value)
    look Inherit{} = Nothing
findBinding _ _ = Nothing

flatten :: NExprLoc -> [NExprLoc]
flatten (Ann _ (NWith _ inner)) = flatten inner
flatten (Ann _ (NBinary NConcat left right)) = flatten left <> flatten right
flatten expr = [expr]

unwrapList :: NExprLoc -> Maybe NExprLoc
unwrapList expr@(Ann _ NList{}) = Just expr
unwrapList (Ann _ (NWith _ inner)) = unwrapList inner
unwrapList _ = Nothing

blocksOf :: Source -> NExprLoc -> [Block]
blocksOf src value = mapMaybe block $ zip (Nothing : fmap (Just . snd . bounds src) operands) operands
  where
    operands = flatten value
    block (previousEnd, operand) = case operand of
        Ann _ (NList elements) -> Just Block{condition = Nothing, list = operand, ..}
        Ann _ (NApp (Ann _ (NApp function condition)) inner)
            | "optionals" `Text.isSuffixOf` slice src function
            , Just list@(Ann _ (NList elements)) <- unwrapList inner ->
                Just Block{condition = Just condition, ..}
        _ -> Nothing

normalised :: Text -> Text
normalised = Text.filter \c -> not (isSpace c || c == '(' || c == ')')

hasSuffix :: [Text] -> Text -> Bool
hasSuffix suffixes element = any (`Text.isSuffixOf` normalised element) suffixes

conjunction :: [Text] -> Text
conjunction = Text.intercalate " && "

attributeText :: [Text] -> Text
attributeText = Text.intercalate "."

bracketed :: Text -> Text
bracketed item = "[ " <> item <> " ]"

optionals :: [Text] -> Text -> Text
optionals condition item = "lib.optionals (" <> conjunction condition <> ") " <> bracketed item

render :: Text -> Element -> Text
render _ Verbatim{text} = text
render qualifier (Package name) = qualifier <> name

sameElement :: Element -> Text -> Bool
sameElement (Package name) element = Text.takeWhileEnd (/= '.') (Text.strip element) == name
sameElement Verbatim{text} element = normalised element == normalised text

bindingPosition :: Binding r -> NSourcePos
bindingPosition (NamedVar _ _ position) = position
bindingPosition (Inherit _ _ position) = position

bindingValueEnd :: Source -> Binding NExprLoc -> Int
bindingValueEnd src (NamedVar _ value _) = snd $ bounds src value
bindingValueEnd src (Inherit _ _ position) = offset src position

semicolonAfter :: Source -> Int -> Int
semicolonAfter Source{text} from = from + Text.length (Text.takeWhile (/= ';') $ Text.drop from text)

appendBinding :: Source -> NExprLoc -> Text -> Either Error Splice
appendBinding src (Ann _ (NSet _ (reverse -> final : _))) binding =
    pure Splice{from = end, to = end, replacement = "\n" <> indentation <> binding}
  where
    end = 1 + semicolonAfter src (bindingValueEnd src final)
    indentation = Text.replicate (snd (coordinates $ bindingPosition final) - 1) " "
appendBinding _ _ _ = Left $ Failed "cannot add to an empty attribute set"

removeRange :: Source -> Int -> Int -> Splice
removeRange Source{text} from to
    | Text.all isSpace before && Text.all isSpace after =
        Splice (from - Text.length before) (to + Text.length after + 1) ""
    | otherwise = Splice from (to + Text.length (Text.takeWhile (== ' ') $ Text.drop to text)) ""
  where
    before = Text.takeWhileEnd (/= '\n') $ Text.take from text
    after = Text.takeWhile (/= '\n') $ Text.drop to text

requiredArguments :: Location -> Element -> [Text]
requiredArguments Location{condition} item =
    mconcat
        [ case item of
            Package _ -> ["pkgs"]
            Verbatim{arguments} -> arguments
        , ["lib" | not $ null condition]
        , ["trilby" | any (Text.isInfixOf "trilby") condition]
        ]

ensureArguments :: Source -> [Text] -> [Splice]
ensureArguments src@Source{text} needed = case innermostAbs src.root of
    Just abstraction@(Ann _ (NAbs (ParamSet _ _ formals) _)) ->
        insertAfter (brace abstraction) $ filter (`notElem` [name | (VarName name, _) <- formals]) needed
    Just _ -> []
    Nothing -> [Splice start start $ "{ " <> Text.intercalate ", " needed <> ", ... }:\n" | not $ null needed]
  where
    start = fst $ bounds src src.root
    brace abstraction = from + 1 + Text.length (Text.takeWhile (/= '{') $ Text.drop from text)
      where
        from = fst $ bounds src abstraction
    insertAfter at missing = [Splice at at $ " " <> Text.intercalate ", " missing <> "," | not $ null missing]

assignment :: Location -> Element -> Text
assignment Location{condition} item = prefix <> body
  where
    prefix = case item of
        Package _ -> "with pkgs; "
        Verbatim{} -> ""
    body
        | null condition = bracketed $ render "" item
        | otherwise = optionals condition $ render "" item

insertElement :: Source -> Text -> Element -> Block -> [Splice]
insertElement src qualifier item Block{list, elements}
    | any (sameElement item . slice src) elements = []
    | null elements = [Splice (listStart + 1) (listEnd - 1) $ " " <> new <> " "]
    | otherwise = [Splice lastEnd lastEnd $ separator <> new]
  where
    new = render qualifier item
    (listStart, listEnd) = bounds src list
    lastElement = last elements
    lastEnd = snd $ bounds src lastElement
    separator
        | fst (beginsAt list) == fst (endsAt list) = " "
        | otherwise = "\n" <> Text.replicate (snd (beginsAt lastElement) - 1) " "

extend :: Source -> Location -> Element -> NExprLoc -> Either Error [Splice]
extend src Location{condition} item value
    | Just block <- find matching blocks = pure $ insertElement src qualifier item block
    | null condition, null blocks = Left $ Failed "no list to extend"
    | null condition = pure [Splice end end $ " ++ " <> bracketed (render qualifier item)]
    | otherwise = pure [Splice end end $ " ++ " <> optionals condition (render qualifier item)]
  where
    blocks = blocksOf src value
    qualifier = case value of
        Ann _ NWith{} -> ""
        _ -> "pkgs."
    end = snd $ bounds src value
    wanted = normalised $ conjunction condition
    matching Block{condition = existing}
        | null condition = isNothing existing
        | otherwise = (normalised . slice src <$> existing) == Just wanted

addElement :: Location -> Element -> Text -> Either Error Text
addElement location@Location{..} item text = do
    src <- parse text
    set <- moduleOf src
    edits <- case findBinding attribute set of
        Just Located{value} -> extend src location item value
        Nothing ->
            pure
                <$> appendBinding src set (attributeText attribute <> " = " <> assignment location item <> ";")
    pure $ splice (ensureArguments src (requiredArguments location item) <> edits) text

elementsOf :: [Text] -> Text -> Either Error [Text]
elementsOf attribute text = do
    src <- parse text
    set <- moduleOf src
    Located{value} <- maybeToEither NotFound $ findBinding attribute set
    pure [slice src element | Block{elements} <- blocksOf src value, element <- elements]

removals :: Source -> (Text -> Bool) -> Block -> [Splice]
removals src matches Block{..}
    | null hits = []
    | length hits == length elements
    , Just before <- previousEnd =
        [Splice before (snd $ bounds src operand) ""]
    | otherwise = uncurry (removeRange src) . bounds src <$> hits
  where
    hits = filter (matches . slice src) elements

removeElement :: [Text] -> (Text -> Bool) -> Text -> Either Error Text
removeElement attribute matches text = do
    src <- parse text
    set <- moduleOf src
    Located{value} <- maybeToEither NotFound $ findBinding attribute set
    case concatMap (removals src matches) $ blocksOf src value of
        [] -> Left NotFound
        edits -> pure $ splice edits text

setValue :: [Text] -> [Text] -> Text -> Text -> Either Error Text
setValue needed attribute new text = do
    src <- parse text
    set <- moduleOf src
    edit <- case findBinding attribute set of
        Just Located{value} ->
            pure Splice{replacement = new, from = fst (bounds src value), to = snd (bounds src value)}
        Nothing -> appendBinding src set $ attributeText attribute <> " = " <> new <> ";"
    pure $ splice (edit : ensureArguments src needed) text

removeBinding :: [Text] -> Text -> Either Error Text
removeBinding attribute text = do
    src <- parse text
    set <- moduleOf src
    Located{..} <- maybeToEither NotFound $ findBinding attribute set
    pure
        $ splice
            [removeRange src (offset src position) (1 + semicolonAfter src (snd $ bounds src value))]
            text
