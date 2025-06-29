module Trilby.Host where

import Data.List.Extra (split)
import Data.Text qualified as Text
import Network.HostName (getHostName)
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import Trilby.App ()
import Trilby.System (System)
import Prelude

data Host
    = Localhost
    | Host {username :: Maybe Text, hostname :: Text}
    deriving stock (Generic, Eq, Ord)

instance IsString Host where
    fromString s =
        case split (== '@') s of
            ((fromString -> Just -> username) : (fromString -> hostname) : _) -> Host{..}
            _ -> Host{username = Nothing, hostname = fromString s}

instance Show Host where
    show Localhost = "localhost"
    show Host{username = Nothing, ..} = Text.unpack hostname
    show Host{username = Just username, ..} = Text.unpack $ username <> "@" <> hostname

hostname :: Host -> App Text
hostname Localhost = liftIO $ fromString <$> getHostName
hostname Host{..} = pure hostname

canonicalHost :: Host -> App Host
canonicalHost host = do
    localhostHostnames <- hostname Localhost <&> (: ["localhost", "127.0.0.1", "::1"])
    isLocalhost <- hostname host <&> (`elem` localhostHostnames)
    pure $ if isLocalhost then Localhost else host

sshFlags :: App (NonEmpty Text)
sshFlags = do
    tmpDir <- view #tmpDir
    pure . sconcat $
        [ ["-o", "ControlMaster=auto"]
        , ["-o", "ControlPath=" <> fromPath (tmpDir </> $(mkRelFile "ssh-%n"))]
        , ["-o", "ControlPersist=60"]
        , ["-t"]
        ]

-- | Execute a command over SSH, if given a remote host.
ssh :: Host -> Process' a -> Process' a
ssh Localhost c t = c t
ssh host c t = do
    flags <- sshFlags
    c $ sconcat [["ssh"], flags, [ishow host], t]

reboot :: App Bool -> Host -> App ()
reboot r host = whenM r $ ssh host runProcess'_ ["sudo", "systemctl", "reboot"]

hostSystem :: Host -> App System
hostSystem host = do
    systemText <-
        fmap firstLine . ssh host readProcessOutText' . sconcat $
            [ ["nix", "eval"]
            , ["--impure"]
            , ["--raw"]
            , ["--expr", "builtins.currentSystem"]
            ]
    pure . read . Text.unpack $ systemText

parseHosts :: Mod ArgumentFields String -> Parser (NonEmpty Host)
parseHosts mods = some1 $ fromString <$> strArgument (metavar "HOST" <> mods)

askHosts :: Maybe (NonEmpty Host) -> App (NonEmpty Host)
askHosts = maybe (pure [Localhost]) pure
