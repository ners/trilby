module Trilby.Config.Target where

import Data.Text qualified as Text
import Effectful.FileSystem.Path.IO (doesFileExist, listDir)
import Path (isProperPrefixOf, stripProperPrefix)
import Trilby.Config.Cache (Cache (..))
import Trilby.Config.Edit (Element (..), Location (..))
import Trilby.Config.Edit qualified as Edit
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory (..))
import Trilby.Config.File qualified as File
import Trilby.Config.Options (Selection (..))
import Trilby.Config.Selector (Selector)
import Trilby.Config.Selector qualified as Selector
import Trilby.Configuration qualified as Configuration
import Trilby.Prelude

data Domain
    = Machine
    | Account
    deriving stock (Eq)

data Destination = Destination
    { file :: Path Abs File
    , domain :: Domain
    }

packagesAttribute :: Domain -> [Text]
packagesAttribute Machine = ["environment", "systemPackages"]
packagesAttribute Account = ["home", "packages"]

currentUser :: (HasCallStack, Environment :> es, IOE :> es, Log :> es) => Eff es Text
currentUser =
    lookupEnv "SUDO_USER"
        >>= maybe (lookupEnv "USER") (pure . Just)
        >>= maybe (errorExit "Cannot determine the current user") (pure . fromString)

cacheOf :: Selection -> Inventory -> Cache
cacheOf Selection{refresh} Inventory{self} = Cache{..}

hostNames :: (Configuring es) => Inventory -> Maybe Selector -> Eff es [Text]
hostNames Inventory{hosts} selector = do
    chosen <- maybe (fromString . Text.unpack <$> Configuration.hostname) pure selector
    case Selector.select chosen hosts of
        [] -> errorExit $ "No hosts match, available hosts: " <> Text.unwords hosts
        matched -> pure matched

localUsers :: (Configuring es) => Path Abs Dir -> Eff es [Text]
localUsers flake = do
    let directory = flake </> $(mkRelDir "users")
    ifM
        (doesDirExist directory)
        ( listDir directory <&> \(directories, _) -> Text.dropWhileEnd (== '/') . fromPath . dirname <$> directories
        )
        (pure [])

domainOf :: Path Abs Dir -> Path Abs File -> Domain
domainOf flake file = case stripProperPrefix flake file of
    Just relative | "users/" `Text.isPrefixOf` fromPath relative -> Account
    _ -> Machine

existing :: (Configuring es) => Text -> Path Abs File -> Eff es (Path Abs File)
existing description file = do
    unlessM (doesFileExist file) . errorExit $ "No such " <> description <> ": " <> fromPath file
    pure file

relativeFile :: Path Abs Dir -> Text -> Eff es (Path Abs File)
relativeFile flake = fmap (flake </>) . parseRelFile . Text.unpack

moduleFile :: (Configuring es) => Path Abs Dir -> Text -> Eff es (Path Abs File)
moduleFile flake relative = do
    asFile <- relativeFile flake relative
    asDirectory <- relativeFile flake $ relative <> "/default.nix"
    ifM (doesFileExist asFile) (pure asFile) (pure asDirectory)

hostFile :: (Configuring es) => Path Abs Dir -> Text -> Eff es Destination
hostFile flake host = do
    file <- existing "host" =<< relativeFile flake ("hosts/" <> host <> "/configuration.nix")
    pure Destination{file, domain = Machine}

homeFile :: (Configuring es) => Path Abs Dir -> Text -> Eff es Destination
homeFile flake user = do
    known <- localUsers flake
    unless (user `elem` known) . errorExit $ "No such user: " <> user
    file <- relativeFile flake $ "users/" <> user <> "/home/default.nix"
    pure Destination{file, domain = Account}

profileFile :: (Configuring es) => Path Abs Dir -> Text -> Eff es Destination
profileFile flake name = do
    candidates <-
        traverse (relativeFile flake) ["profiles/" <> name <> ".nix", "profiles/" <> name <> "/default.nix"]
    found <- filterM doesFileExist candidates
    case found of
        file : _ -> pure Destination{file, domain = Machine}
        [] -> errorExit $ "No such local profile: " <> name

explicitFile :: (Configuring es) => Path Abs Dir -> FilePath -> Eff es Destination
explicitFile flake path = do
    file <-
        maybe (errorExit "Invalid file path") pure $ parseAbsFile path <|> (flake </>) <$> parseRelFile path
    unless (flake `isProperPrefixOf` file) $ errorExit "The file is not inside the flake"
    pure Destination{file, domain = domainOf flake file}

destinations
    :: (Configuring es)
    => Selection
    -> Maybe FilePath
    -> Inventory
    -> Eff es [Destination]
destinations Selection{..} explicit inventory
    | Just path <- explicit = (: []) <$> explicitFile flake path
    | Just name <- profile = (: []) <$> profileFile flake name
    | Just selector <- users = localUsers flake >>= traverse (homeFile flake) . Selector.select selector
    | Just selector <- hosts = hostNames inventory (Just selector) >>= traverse (hostFile flake)
    | otherwise = (: []) <$> (homeFile flake =<< currentUser)

homeModule :: NExpr
homeModule = [nix| { pkgs, ... }: { home.packages = with pkgs; [ ]; } |]

createHome :: (Configuring es) => Destination -> Eff es ()
createHome Destination{file, domain = Account}
    | "/home/default.nix" `Text.isSuffixOf` fromPath file = unlessM (doesFileExist file) do
        File.create file homeModule
        File.modify userModule
            . Edit.addElement Location{attribute = ["imports"], condition = []}
            $ Verbatim{arguments = [], text = "./home"}
  where
    userModule = parent (parent file) </> $(mkRelFile "default.nix")
createHome _ = pure ()
