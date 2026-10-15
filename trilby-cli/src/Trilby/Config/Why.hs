module Trilby.Config.Why where

import Data.List (intercalate, nub, sortOn)
import Data.List.Extra (nubOrd, nubOrdOn, nubSort)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Kind, Listing (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Options (Selection (..), Visible (..), WhyOpts (..))
import Trilby.Config.Origin (Roots)
import Trilby.Config.Origin qualified as Origin
import Trilby.Config.Rows (Candidate (..))
import Trilby.Config.Rows qualified as Rows
import Trilby.Config.Selector qualified as Selector
import Trilby.Config.Table (Row (..), Scope (..))
import Trilby.Config.Table qualified as Table
import Trilby.Config.Target qualified as Target
import Trilby.Prelude

data Finding = Finding
    { kind :: Kind
    , scope :: Scope
    , name :: Text
    , path :: Text
    , roots :: Roots
    }

dropRoot :: [Text] -> [Text]
dropRoot (root : rest@(_ : _))
    | Text.takeWhileEnd (/= '/') root == "flake.nix" = rest
dropRoot chain = chain

tidy :: [Text] -> [Text]
tidy = fmap NonEmpty.head . NonEmpty.group . dropRoot

renderChain :: [Text] -> [Text]
renderChain = zipWith line [0 :: Int ..]
  where
    line 0 link = "  " <> link
    line depth link = "  " <> Text.replicate (2 * (depth - 1)) " " <> "└─ " <> link

heading :: Kind -> Scope -> Text -> Text
heading kind scope name = Text.toLower (Rows.title kind) <> " " <> name <> "  " <> Table.label True scope

renderFinding :: Text -> Maybe [Text] -> [Text]
renderFinding header (Just chain@(_ : _)) = header : renderChain chain
renderFinding header _ = [header, "  (no import path found)"]

link :: Roots -> Text -> Text
link roots path = file <> " (" <> ishow origin <> ")"
  where
    (origin, file) = Origin.locate roots path

why :: (Configuring es) => WhyOpts -> Eff es ()
why WhyOpts{selection = selection@Selection{flake}, kinds, item = wanted} = do
    inventory <- Eval.inventory flake
    let cache = Target.cacheOf selection inventory
    hosts <- Target.hostNames inventory selection.hosts
    users <- case selection.users of
        Nothing -> (: []) <$> Target.currentUser
        Just selector ->
            Selector.select selector . nub . mconcat . Map.elems <$> Eval.users cache flake hosts
    found <- fmap mconcat . for kinds $ \kind -> do
        listings <- Eval.listings kind cache flake hosts users
        pure
            [ Finding{name = item.row.name, scope = item.row.scope, path = item.path, ..}
            | (host, listing@Listing{nixpkgs}) <- Map.toList listings
            , let roots = Eval.roots inventory nixpkgs
            , item <- Rows.listingItems kind inventory mempty Everything host listing
            , Rows.named wanted item
            ]
    let findings = nubOrdOn (\f -> (f.kind, f.scope, f.name, f.path)) found
    when (null findings)
        . errorExit
        $ "Nothing called "
        <> wanted
        <> " for the selected hosts and users"
    let scopes = nubOrd $ (.scope) <$> findings
    traced <- for scopes \scope@Scope{host, user} ->
        (scope,)
            <$> Eval.chains flake host user (nubSort [f.path | f <- findings, f.scope == scope])
    let chainOf Finding{scope, path} = tidy <$> (join . Map.lookup path =<< lookup scope traced)
    liftIO
        . Text.putStr
        . Text.unlines
        . intercalate [""]
        $ [ renderFinding (heading kind scope name) (fmap (link roots) <$> chainOf f)
          | f@Finding{..} <- sortOn (\f -> (f.kind, f.scope, f.name)) findings
          ]
