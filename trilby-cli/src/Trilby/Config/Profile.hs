module Trilby.Config.Profile where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Effectful.FileSystem.Path.IO (doesFileExist)
import Path (stripProperPrefix)
import System.FilePath (splitDirectories)
import Trilby.Config.Edit (Element (..))
import Trilby.Config.Edit qualified as Edit
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory (..))
import Trilby.Config.Origin (Origin (..), Roots)
import Trilby.Config.Origin qualified as Origin
import Trilby.Prelude

data Found = Found
    { origin :: Origin
    , name :: Text
    , relative :: Text
    }

directory :: Origin -> Maybe Text
directory Local = Just "profiles"
directory Trilby = Just "modules/profiles"
directory Nixos = Just "nixos/modules/profiles"
directory Input{} = Nothing

nameOf :: Roots -> Text -> Maybe (Origin, Text)
nameOf roots path = do
    let (origin, relative) = Origin.locate roots path
    base <- directory origin
    rest <- Text.stripPrefix (base <> "/") relative
    pure
        (origin, fromMaybe rest $ Text.stripSuffix "/default.nix" rest <|> Text.stripSuffix ".nix" rest)

rootOf :: Path Abs Dir -> Inventory -> Text -> Origin -> Maybe Text
rootOf flake _ _ Local = Just . Text.dropWhileEnd (== '/') $ fromPath flake
rootOf _ Inventory{inputs} _ Trilby = Map.lookup "trilby" inputs
rootOf _ _ nixpkgs Nixos = Just nixpkgs
rootOf _ _ _ Input{} = Nothing

find :: (Configuring es) => Path Abs Dir -> Inventory -> Text -> Text -> Eff es (Maybe Found)
find flake inventory nixpkgs name = findM present candidates
  where
    exists :: (FileSystem :> es') => Text -> Eff es' Bool
    candidates =
        [ Found{origin, name, relative}
        | origin <- [Local, Trilby, Nixos]
        , Just dir <- [directory origin]
        , relative <- [dir <> "/" <> name <> ".nix", dir <> "/" <> name]
        ]
    present Found{origin, relative} =
        case (rootOf flake inventory nixpkgs origin, Text.stripSuffix ".nix" relative) of
            (Just root, Just _) -> exists $ root <> "/" <> relative
            (Just root, Nothing) -> exists $ root <> "/" <> relative <> "/default.nix"
            (Nothing, _) -> pure False
    exists path = maybe (pure False) doesFileExist . parseAbsFile $ Text.unpack path

localImport :: Path Abs Dir -> Path Abs File -> Text -> Text
localImport flake destination relative = Text.intercalate "/" $ prefix <> [relative]
  where
    depth = maybe 0 (length . splitDirectories . toFilePath) $ stripProperPrefix flake $ parent destination
    prefix
        | depth == 0 = ["."]
        | otherwise = replicate depth ".."

element :: Path Abs Dir -> Path Abs File -> Found -> Either Text Element
element flake destination Found{origin = Local, relative} =
    Right Verbatim{arguments = [], text = localImport flake destination relative}
element _ _ Found{origin = Trilby, name}
    | "/" `Text.isInfixOf` name = Left "Nested Trilby profiles cannot be addressed"
    | otherwise =
        Right
            Verbatim
                { arguments = ["trilby"]
                , text = "trilby.inputs.self.nixosModules.profiles." <> name
                }
element _ _ Found{origin = Nixos, name} =
    Right
        Verbatim
            { arguments = ["modulesPath"]
            , text = "(modulesPath + \"/profiles/" <> name <> ".nix\")"
            }
element _ _ Found{origin = Input{}} = Left "Profiles from inputs cannot be addressed"

imports :: Text -> Text -> Bool
imports name =
    Edit.hasSuffix
        [ "profiles." <> name
        , "profiles/" <> name
        , "profiles/" <> name <> ".nix"
        , "profiles/" <> name <> ".nix\""
        ]
