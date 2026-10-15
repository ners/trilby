module Trilby.Config.Options where

import Data.Text qualified as Text
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import Trilby.Config.Cache (Refresh (..))
import Trilby.Config.Eval (Kind (..), allKinds, kindName)
import Trilby.Config.Origin (Origin)
import Trilby.Config.Selector (Selector)
import Trilby.Install.Config.Edition (Edition)
import Trilby.Prelude
import Trilby.Update.Options (UpdateAction, parseAction)

data Visible
    = Everything
    | SystemOnly
    | UsersOnly

data Selection = Selection
    { flake :: Path Abs Dir
    , hosts :: Maybe Selector
    , users :: Maybe Selector
    , profile :: Maybe Text
    , refresh :: Refresh
    }

data ListOpts = ListOpts
    { selection :: Selection
    , visible :: Visible
    , from :: [Origin]
    , files :: Bool
    , unique :: Bool
    , json :: Bool
    , size :: Bool
    , fullPath :: Bool
    }

data ChangeOpts m = ChangeOpts
    { selection :: Selection
    , file :: Maybe FilePath
    , only :: [Text]
    , yes :: Bool
    , apply :: m (UpdateAction m)
    , items :: NonEmpty Text
    }

data Operation m
    = ListEntries ListOpts
    | AddEntries (ChangeOpts m)
    | RemoveEntries (ChangeOpts m)

data HostsOpts m
    = ListHosts {flake :: Path Abs Dir}
    | AddHost
        { flake :: Path Abs Dir
        , hostname :: Text
        , edition :: m Edition
        , platform :: Maybe Text
        }

data DiffOpts = DiffOpts
    { flake :: Path Abs Dir
    , refresh :: Refresh
    , left :: Text
    , right :: Text
    , kinds :: [Kind]
    , users :: Maybe Selector
    , from :: [Origin]
    , json :: Bool
    }

data SearchOpts = SearchOpts
    { selection :: Selection
    , visible :: Visible
    , kinds :: [Kind]
    , exact :: Bool
    , from :: [Origin]
    , files :: Bool
    , json :: Bool
    , query :: Text
    }

data WhyOpts = WhyOpts
    { selection :: Selection
    , kinds :: [Kind]
    , item :: Text
    }

data EditOpts m = EditOpts
    { selection :: Selection
    , file :: Maybe FilePath
    , apply :: m (UpdateAction m)
    }

data CheckOpts = CheckOpts
    { flake :: Path Abs Dir
    , hosts :: Maybe Selector
    , warnings :: Bool
    }

data ConfigOpts m
    = Entities Kind (Operation m)
    | Users (Operation m)
    | Hosts (HostsOpts m)
    | DiffHosts DiffOpts
    | SearchEntries SearchOpts
    | WhyEntry WhyOpts
    | EditFile (EditOpts m)
    | CheckHosts CheckOpts

restrictions :: [(String, Text)]
restrictions =
    [ ("workstation", "trilby.edition == \"workstation\"")
    , ("server", "trilby.edition == \"server\"")
    , ("linux", "trilby.hostSystem.kernel.name == \"linux\"")
    , ("darwin", "trilby.hostSystem.kernel.name == \"darwin\"")
    ]

parseFlake :: Parser (Path Abs Dir)
parseFlake =
    parsePath parseAbsDir
        $ long "flake"
        <> metavar "DIR"
        <> value (trilbyHome rootDir)
        <> showDefaultWith toFilePath
        <> help "The flake to configure"

parseSelection :: Parser Selection
parseSelection = do
    flake <- parseFlake
    hosts <-
        optional . option str $ long "host" <> metavar "GLOBS" <> help "Hosts to act on, default: this host"
    users <-
        optional . option str $ long "user" <> metavar "GLOBS" <> help "Users to act on, default: you"
    profile <- optional . strOption $ long "profile" <> metavar "NAME" <> help "Profile to act on"
    refresh <- parseRefresh
    pure Selection{..}

parseRefresh :: Parser Refresh
parseRefresh = flag UseCache Refresh $ long "refresh" <> help "Evaluate again instead of using cached results"

kindNames :: [(String, Kind)]
kindNames = [(Text.unpack $ kindName kind, kind) | kind <- allKinds]

unknownKind :: Text -> String
unknownKind name =
    "unknown kind "
        <> show name
        <> ", expected a comma-separated list of "
        <> Text.unpack (Text.intercalate ", " $ Text.pack . fst <$> kindNames)

readKinds :: String -> Either String [Kind]
readKinds = traverse readKind . Text.splitOn "," . Text.pack
  where
    readKind name = maybe (Left $ unknownKind name) Right . lookup (Text.unpack name) $ kindNames

parseKinds :: Parser [Kind]
parseKinds =
    option (eitherReader readKinds)
        $ long "kind"
        <> metavar "KINDS"
        <> value allKinds
        <> completeWith (fst <$> kindNames)
        <> help "Only consider these comma-separated kinds: packages, services, profiles. Default: all"

parseHostGlobs :: Parser (Maybe Selector)
parseHostGlobs =
    optional
        . option str
        $ long "host"
        <> metavar "GLOBS"
        <> help "Hosts to act on, default: all of them"

parseVisible :: Parser Visible
parseVisible =
    flag' SystemOnly (long "system" <> help "Only show system entries")
        <|> flag' UsersOnly (long "no-system" <> help "Only show user entries")
        <|> pure Everything

parseFrom :: Parser [Origin]
parseFrom =
    option (eitherReader $ Right . fmap (fromString . Text.unpack) . Text.splitOn "," . Text.pack)
        $ long "from"
        <> metavar "ORIGINS"
        <> value []
        <> help "Only show entries from these origins: local, trilby, nixos, or an input name"

parseFiles :: Parser Bool
parseFiles = switch $ long "files" <> help "Show the file that defines each entry"

parseUnique :: Parser Bool
parseUnique = switch $ long "unique" <> help "Only list each name once, across hosts, users and origins"

parseJson :: Parser Bool
parseJson = switch $ long "json" <> help "Print the entries as a JSON array"

parseSize :: Parser Bool
parseSize = switch $ long "size" <> help "Show the size of each package in the local store"

parseFullPath :: Parser Bool
parseFullPath = switch $ long "full-path" <> help "Show the store path of each package instead of its name"

parseFile :: Parser (Maybe FilePath)
parseFile = optional . strOption $ long "file" <> metavar "PATH" <> help "The file to edit"

parseOnly :: Parser [Text]
parseOnly =
    many
        . option (maybeReader (`lookup` restrictions))
        $ long "only"
        <> metavar "RESTRICTION"
        <> completeWith (fst <$> restrictions)
        <> help "Restrict packages to workstation, server, linux or darwin"

parseYes :: Parser Bool
parseYes = switch $ long "yes" <> help "Do not ask for confirmation"

parseItems :: Parser (NonEmpty Text)
parseItems = some1 $ strArgument $ metavar "NAME"

parseOperation :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (Operation m)
parseOperation f =
    hsubparser
        . mconcat
        $ [ command "list" $ info (ListEntries <$> parseList) (progDesc "List entries")
          , command "add" $ info (AddEntries <$> parseAdd) (progDesc "Add entries")
          , command "rm" $ info (RemoveEntries <$> parseRemove) (progDesc "Remove entries")
          ]
  where
    parseList =
        ListOpts
            <$> parseSelection
            <*> parseVisible
            <*> parseFrom
            <*> parseFiles
            <*> parseUnique
            <*> parseJson
            <*> parseSize
            <*> parseFullPath
    parseAdd = do
        selection <- parseSelection
        file <- parseFile
        only <- parseOnly
        yes <- parseYes
        apply <- f $ parseAction f
        items <- parseItems
        pure ChangeOpts{..}
    parseRemove = do
        selection <- parseSelection
        file <- parseFile
        yes <- parseYes
        apply <- f $ parseAction f
        items <- parseItems
        pure ChangeOpts{only = [], ..}

parseHosts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (HostsOpts m)
parseHosts f =
    hsubparser
        . mconcat
        $ [ command "list" $ info (ListHosts <$> parseFlake) (progDesc "List hosts")
          , command "add" $ info parseAdd (progDesc "Add a host")
          ]
  where
    parseAdd = do
        flake <- parseFlake
        edition <-
            f $ parseEnum (long "edition" <> metavar "EDITION" <> help "The Trilby edition of the host")
        platform <- optional . strOption $ long "platform" <> metavar "PLATFORM" <> help "The host platform"
        hostname <- strArgument $ metavar "HOSTNAME"
        pure AddHost{..}

parseOpts :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (ConfigOpts m)
parseOpts f =
    hsubparser
        . mconcat
        $ [ entities "packages" Packages
          , entities "profiles" Profiles
          , entities "services" Services
          , command "users" $ info (Users <$> parseOperation f) (progDesc "Manage the users of hosts")
          , command "hosts" $ info (Hosts <$> parseHosts f) (progDesc "Manage hosts")
          , command "diff" $ info (DiffHosts <$> parseDiff) (progDesc "Compare two hosts")
          , command "search"
                $ info (SearchEntries <$> parseSearch) (progDesc "Find which hosts and users have an entry")
          , command "why" $ info (WhyEntry <$> parseWhy) (progDesc "Explain where an entry comes from")
          , command "edit" $ info (EditFile <$> parseEdit f) (progDesc "Edit a host, user or profile")
          , command "check" $ info (CheckHosts <$> parseCheck) (progDesc "Check that hosts evaluate")
          ]
  where
    entities name kind =
        command name $ info (Entities kind <$> parseOperation f) (progDesc $ "Manage " <> name)

parseDiff :: Parser DiffOpts
parseDiff = do
    flake <- parseFlake
    refresh <- parseRefresh
    kinds <- parseKinds
    users <-
        optional
            . option str
            $ long "user"
            <> metavar "GLOBS"
            <> help "Also compare the entries of these users"
    from <- parseFrom
    json <- parseJson
    left <- strArgument $ metavar "HOST"
    right <- strArgument $ metavar "HOST"
    pure DiffOpts{..}

parseSearch :: Parser SearchOpts
parseSearch = do
    selection <- parseSelection
    visible <- parseVisible
    kinds <- parseKinds
    exact <- switch $ long "exact" <> help "Only match names that are equal to the query"
    from <- parseFrom
    files <- parseFiles
    json <- parseJson
    query <- strArgument $ metavar "QUERY"
    pure SearchOpts{..}

parseWhy :: Parser WhyOpts
parseWhy = do
    selection <- parseSelection
    kinds <- parseKinds
    item <- strArgument $ metavar "NAME"
    pure WhyOpts{..}

parseEdit :: forall m. (forall a. Parser a -> Parser (m a)) -> Parser (EditOpts m)
parseEdit f = do
    selection <- parseSelection
    file <- parseFile
    apply <- f $ parseAction f
    pure EditOpts{..}

parseCheck :: Parser CheckOpts
parseCheck = do
    flake <- parseFlake
    hosts <- parseHostGlobs
    warnings <- switch $ long "warnings" <> help "Show the evaluation warnings of each host"
    pure CheckOpts{..}
