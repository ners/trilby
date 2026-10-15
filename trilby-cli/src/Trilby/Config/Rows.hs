module Trilby.Config.Rows where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Trilby.Config.Eval (Entry (..), Inventory, Kind (..), Listing (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Options (Visible (..))
import Trilby.Config.Origin (Origin (..), Roots)
import Trilby.Config.Origin qualified as Origin
import Trilby.Config.Profile qualified as Profile
import Trilby.Config.Table (Row (..), Scope (..))
import Trilby.Prelude

data Candidate = Candidate
    { key :: Text
    , path :: Text
    , row :: Row
    }

title :: Kind -> Text
title Packages = "PACKAGE"
title Services = "SERVICE"
title Profiles = "PROFILE"

itemsOf :: Kind -> Roots -> Map Text Integer -> Scope -> [Entry] -> [Candidate]
itemsOf Profiles roots _ scope entries =
    [ Candidate
        { key = name
        , row = Row{file = Just . snd $ Origin.locate roots path, size = Nothing, ..}
        , ..
        }
    | Entry{files} <- entries
    , path <- files
    , Just (origin, name) <- [Profile.nameOf roots path]
    ]
itemsOf _ roots sizes scope entries =
    [ Candidate{key = fromMaybe name pname, row = Row{..}, ..}
    | Entry{name, pname, storePath, files} <- entries
    , path <- files
    , let (origin, located) = Origin.locate roots path
    , let file = Just located
    , let size = (`Map.lookup` sizes) =<< storePath
    ]

listingItems :: Kind -> Inventory -> Map Text Integer -> Visible -> Text -> Listing -> [Candidate]
listingItems kind inventory sizes visible host Listing{nixpkgs, system, users} = case visible of
    Everything -> systemItems <> userItems
    SystemOnly -> systemItems
    UsersOnly -> userItems
  where
    roots = Eval.roots inventory nixpkgs
    systemItems = itemsOf kind roots sizes Scope{host, user = Nothing} system
    userItems =
        mconcat
            [ itemsOf kind roots sizes Scope{host, user = Just user} entries
            | (user, entries) <- Map.toList users
            ]

named :: Text -> Candidate -> Bool
named wanted Candidate{key, row = Row{name}} = key == wanted || name == wanted

ownedBy :: Text -> Row -> Bool
ownedBy profile Row{origin, file} =
    origin
        == Local
        && ( file
                == Just ("profiles/" <> profile <> ".nix")
                || file
                == Just ("profiles/" <> profile <> "/default.nix")
           )

fromOrigins :: [Origin] -> Row -> Bool
fromOrigins origins Row{origin} = null origins || origin `elem` origins
