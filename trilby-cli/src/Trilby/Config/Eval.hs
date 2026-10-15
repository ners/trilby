module Trilby.Config.Eval where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.Extra (nubSort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Effectful.Concurrent.Async (pooledForConcurrentlyN)
import Numeric (showFFloat)
import Trilby.Config.Cache (Cache (..))
import Trilby.Config.Cache qualified as Cache
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Origin (Origin (..), Roots)
import Trilby.Config.Progress qualified as Progress
import Trilby.HNix (showNix)
import Trilby.Prelude

data Kind
    = Packages
    | Services
    | Profiles
    deriving stock (Eq, Ord, Enum, Bounded)

allKinds :: [Kind]
allKinds = [minBound .. maxBound]

data Inventory = Inventory
    { self :: Text
    , inputs :: Map Text Text
    , hosts :: [Text]
    , darwinHosts :: [Text]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data Detail
    = NamesOnly
    | WithPaths
    deriving stock (Eq)

data Entry = Entry
    { name :: Text
    , pname :: Maybe Text
    , storePath :: Maybe Text
    , files :: [Text]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data Listing = Listing
    { nixpkgs :: Text
    , system :: [Entry]
    , users :: Map Text [Entry]
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

maxConcurrentHosts :: Int
maxConcurrentHosts = 4

kindName :: Kind -> Text
kindName Packages = "packages"
kindName Services = "services"
kindName Profiles = "profiles"

program :: Path Abs Dir -> NExpr -> NExpr
program flakePath body =
    [nix|
    let
      flake = builtins.getFlake flakePath;
      configurations = (flake.nixosConfigurations or { }) // (flake.darwinConfigurations or { });
      isOption = x: builtins.isAttrs x && (x._type or null) == "option";
      unwrap = v:
        if builtins.isAttrs v && builtins.elem (v._type or null) [ "if" "override" "order" ]
        then (if v._type == "if" && !v.condition then [ ] else unwrap v.content)
        else [ v ];
      describe = withPaths: p:
        if builtins.isAttrs p && p ? name
        then {
          name = p.name;
          pname = p.pname or null;
          storePath = if withPaths then p.outPath or null else null;
        }
        else { name = "?"; };
      listPackages = withPaths: option: builtins.concatMap
        (d: builtins.map
          (p: describe withPaths p // { files = [ d.file ]; })
          (builtins.concatMap unwrap d.value))
        option.definitionsWithLocations;
      listServices = services: builtins.concatMap
        (name:
          let service = services.${name}; in
          if builtins.isAttrs service && service ? enable && isOption service.enable
            && service.enable.isDefined && service.enable.value == true
          then [{
            inherit name;
            files = builtins.map (d: d.file)
              (builtins.filter (d: d.value == true) service.enable.definitionsWithLocations);
          }]
          else [ ])
        (builtins.attrNames services);
      listFiles = graph:
        let files = g: builtins.concatMap (m: [ m.file ] ++ files m.imports) g; in
        builtins.map (file: { name = file; files = [ file ]; })
          (builtins.attrNames
            (builtins.listToAttrs (builtins.map (file: { name = file; value = null; }) (files graph))));
      homeOf = c: user: (c.options.home-manager.users.valueMeta.attrs or { }).${user} or null;
    in body { inherit flake configurations homeOf listPackages listServices listFiles; }
    |]

truncated :: Int -> Text -> Text
truncated limit text
    | Text.length text > limit = Text.take limit text <> "..."
    | otherwise = text

failureReason :: Text -> Text
failureReason stderr = case break (Text.isInfixOf "error:") . reverse $ Text.lines stderr of
    (_, []) -> "nix eval failed"
    (following, line : _) ->
        truncated 300
            . Text.unwords
            . Text.words
            . Text.unlines
            $ Text.drop (Text.length "error:") (snd $ Text.breakOn "error:" line)
            : reverse following

data Run = Run
    { code :: ExitCode
    , out :: LazyByteString
    , stderr :: Text
    }

runNixRaw :: (Configuring es) => NonEmpty Text -> Eff es Run
runNixRaw args = do
    command <- nixCommand args
    (code, out, err) <- readProcess . setStdin nullStream $ proc command
    let stderr = Text.decodeUtf8 $ LazyByteString.toStrict err
    unless (Text.null stderr) $ logInfo_ stderr
    pure Run{..}

runNix :: (Configuring es, FromJSON a) => NonEmpty Text -> Eff es (Either Text a)
runNix command =
    runNixRaw command <&> \Run{..} -> case code of
        ExitSuccess -> Bifunctor.first Text.pack $ Aeson.eitherDecode out
        ExitFailure _ -> Left $ failureReason stderr

expression :: Path Abs Dir -> NExpr -> Text
expression flake body = showNix $ program flake body

evalCommand :: Path Abs Dir -> NExpr -> NonEmpty Text
evalCommand flake body = ["eval", "--impure", "--json", "--expr", expression flake body]

attempt :: (Configuring es, FromJSON a) => Path Abs Dir -> NExpr -> Eff es (Either Text a)
attempt flake body = runNix $ evalCommand flake body

attemptRaw :: (Configuring es) => Path Abs Dir -> NExpr -> Eff es Run
attemptRaw flake body = runNixRaw $ evalCommand flake body

data Query = Query
    { flake :: Path Abs Dir
    , cache :: Cache
    , label :: Text
    , body :: NExpr
    }

queryKey :: Query -> Text
queryKey Query{..} = cache.self <> "\n" <> expression flake body

queryLabel :: Query -> Text
queryLabel Query{..} = Text.pack (fromPath flake) <> "." <> label

cachedResult :: (Configuring es, FromJSON a) => Query -> Eff es (Maybe a)
cachedResult query =
    Cache.fetch query.cache (queryLabel query) (queryKey query) <&> \hit ->
        hit >>= Aeson.parseMaybe Aeson.parseJSON

attemptCached :: (Configuring es, FromJSON a) => Query -> Eff es (Either Text a)
attemptCached query@Query{flake, body} = do
    result <- attempt @_ @Aeson.Value flake body
    for_ result $ Cache.store (queryLabel query) (queryKey query)
    pure $ result >>= Bifunctor.first Text.pack . Aeson.parseEither Aeson.parseJSON

newtype PathInfo = PathInfo {narSize :: Integer}
    deriving stock (Generic)
    deriving anyclass (FromJSON)

newtype PathInfos = PathInfos {info :: Map Text (Maybe PathInfo)}
    deriving stock (Generic)
    deriving anyclass (FromJSON)

narSizes :: (Configuring es) => [Text] -> Eff es (Map Text Integer)
narSizes [] = pure mempty
narSizes paths = do
    PathInfos{info} <-
        either errorExit pure =<< runNix ("path-info" :| ["--json", "--json-format", "2"] <> paths)
    pure
        $ Map.fromList
            [ (path, narSize)
            | path <- paths
            , Just PathInfo{narSize} <- [join $ Map.lookup (Text.takeWhileEnd (/= '/') path) info]
            ]

evaluate :: (Configuring es, FromJSON a) => Path Abs Dir -> NExpr -> Eff es a
evaluate flake body = attempt flake body >>= either errorExit pure

seconds :: (Real a) => a -> Text
seconds elapsed = Text.pack $ showFFloat (Just 1) (realToFrac elapsed :: Double) "s"

timed :: (IOE :> es) => Eff es a -> Eff es (a, NominalDiffTime)
timed action = do
    started <- liftIO getCurrentTime
    result <- action
    elapsed <- liftIO $ (`diffUTCTime` started) <$> getCurrentTime
    pure (result, elapsed)

perHostResults
    :: (Configuring es)
    => [Text]
    -> (Text -> Eff es (Maybe a))
    -> (Text -> Eff es (Either Text a))
    -> Eff es (Map Text (Either Text a))
perHostResults selectedHosts cachedHost evaluateHost = do
    progress <- Progress.new
    Map.fromList <$> pooledForConcurrentlyN maxConcurrentHosts selectedHosts \host ->
        (host,) <$> do
            cachedHost host >>= \case
                Just value -> do
                    logInfo_ $ "Using cached evaluation of host " <> host
                    pure $ Right value
                Nothing -> do
                    line <- Progress.start progress $ "Evaluating host " <> host <> " ..."
                    (result, elapsed) <- timed $ evaluateHost host
                    Progress.finish progress line case result of
                        Left reason -> "Failed host " <> host <> ": " <> truncated 100 reason
                        Right _ -> "Evaluated host " <> host <> " in " <> seconds elapsed
                    pure result

perHost :: (Configuring es, FromJSON a) => [Text] -> (Text -> Query) -> Eff es (Map Text a)
perHost selectedHosts query =
    Map.mapMaybe (either (const Nothing) Just)
        <$> perHostResults selectedHosts (cachedResult . query) (attemptCached . query)

inventory :: (Configuring es) => Path Abs Dir -> Eff es Inventory
inventory flake =
    evaluate
        flake
        [nix|
        { flake, configurations, ... }: {
          self = flake.outPath;
          inputs = builtins.mapAttrs (_: input: input.outPath) flake.inputs;
          hosts = builtins.attrNames configurations;
          darwinHosts = builtins.attrNames (flake.darwinConfigurations or { });
        }
        |]

roots :: Inventory -> Text -> Roots
roots Inventory{..} nixpkgs =
    mconcat
        [ [(self, Local), (nixpkgs, Nixos)]
        , [(path, fromString $ Text.unpack name) | (name, path) <- Map.toList inputs]
        ]

detailName :: Detail -> Text
detailName NamesOnly = "names"
detailName WithPaths = "paths"

listingQuery :: Kind -> Detail -> Cache -> Path Abs Dir -> [Text] -> Text -> Query
listingQuery kind detail cache flake selectedUsers host =
    Query
        { label =
            Text.intercalate
                "."
                [host, kindName kind, detailName detail, Text.intercalate "+" $ nubSort selectedUsers]
        , body =
            [nix|
        { configurations, homeOf, listPackages, listServices, listFiles, ... }:
        let
          c = configurations.${host};
          entries = ({
            packages = {
              system = e: listPackages withPaths e.options.environment.systemPackages;
              home = e: listPackages withPaths e.options.home.packages;
            };
            services = {
              system = e: listServices e.options.services;
              home = e: listServices e.options.services;
            };
            profiles = {
              system = e: listFiles e.graph;
              home = e: listFiles e.graph;
            };
          }).${selected};
        in {
          nixpkgs = c.pkgs.path;
          system = entries.system c;
          users = builtins.listToAttrs (builtins.concatMap (user:
            let h = homeOf c user; in
            if h == null then [ ] else [{ name = user; value = entries.home h.configuration; }]
          ) selectedUsers);
        }
        |]
        , ..
        }
  where
    selected = kindName kind
    withPaths = detail == WithPaths

listingsWith
    :: (Configuring es)
    => Detail
    -> Kind
    -> Cache
    -> Path Abs Dir
    -> [Text]
    -> [Text]
    -> Eff es (Map Text Listing)
listingsWith detail kind cache flake selectedHosts selectedUsers =
    perHost selectedHosts $ listingQuery kind detail cache flake selectedUsers

listings
    :: (Configuring es)
    => Kind
    -> Cache
    -> Path Abs Dir
    -> [Text]
    -> [Text]
    -> Eff es (Map Text Listing)
listings = listingsWith NamesOnly

usersQuery :: Cache -> Path Abs Dir -> Text -> Query
usersQuery cache flake host =
    Query
        { label = host <> ".users"
        , body =
            [nix|
            { configurations, ... }:
            builtins.attrNames (configurations.${host}.options.home-manager.users.valueMeta.attrs or { })
            |]
        , ..
        }

users :: (Configuring es) => Cache -> Path Abs Dir -> [Text] -> Eff es (Map Text [Text])
users cache flake selectedHosts = perHost selectedHosts $ usersQuery cache flake

nixpkgs :: (Configuring es) => Path Abs Dir -> Text -> Eff es Text
nixpkgs flake host =
    evaluate
        flake
        [nix|
        { configurations, ... }: configurations.${host}.pkgs.path
        |]

packageExists :: (Configuring es) => Path Abs Dir -> Text -> Text -> Eff es Bool
packageExists flake host package =
    evaluate
        flake
        [nix|
        { configurations, ... }:
        builtins.foldl'
          (set: key: if builtins.isAttrs set && set ? ${key} then set.${key} else null)
          configurations.${host}.pkgs
          segments != null
        |]
  where
    segments = Text.splitOn "." package

serviceExists :: (Configuring es) => Path Abs Dir -> Text -> Text -> Eff es Bool
serviceExists flake host service =
    evaluate
        flake
        [nix|
        { configurations, ... }:
        (configurations.${host}.options.services.${service} or { }) ? enable
        |]

systemDerivation :: Text -> NExpr
systemDerivation host =
    [nix|
    { flake, configurations, ... }:
    (if builtins.hasAttr host (flake.darwinConfigurations or { })
      then configurations.${host}.system
      else configurations.${host}.config.system.build.toplevel).drvPath
    |]

chainQuery :: Text -> Maybe Text -> [Text] -> NExpr
chainQuery host user targets =
    [nix|
    { configurations, homeOf, ... }:
    let
      c = configurations.${host};
      home = homeOf c user;
      graph = if user == null then c.graph else home.configuration.graph;
      walk = trail: node:
        let current = trail ++ [ node.file ]; in
        [ { file = node.file; chain = current; } ] ++ builtins.concatMap (walk current) node.imports;
      hits = builtins.filter (x: builtins.elem x.file targets) (builtins.concatMap (walk [ ]) graph);
      shortest = builtins.foldl'
        (acc: x:
          if builtins.hasAttr x.file acc && builtins.length acc.${x.file} <= builtins.length x.chain
          then acc
          else acc // builtins.listToAttrs [{ name = x.file; value = x.chain; }])
        { }
        hits;
    in builtins.listToAttrs
      (builtins.map (t: { name = t; value = shortest.${t} or null; }) targets)
    |]

chains
    :: (Configuring es)
    => Path Abs Dir
    -> Text
    -> Maybe Text
    -> [Text]
    -> Eff es (Map Text (Maybe [Text]))
chains flake host user targets = evaluate flake $ chainQuery host user targets
