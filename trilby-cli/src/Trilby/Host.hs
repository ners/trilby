module Trilby.Host where

import Data.List.Extra (split)
import Data.Text qualified as Text
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
hostname Localhost = view #hostname
hostname Host{..} = pure hostname

canonicalHost :: Host -> App Host
canonicalHost host = do
    localhostHostnames <- view #hostname <&> (: ["localhost", "127.0.0.1", "::1"])
    isLocalhost <- hostname host <&> (`elem` localhostHostnames)
    pure $ if isLocalhost then Localhost else host

-- | Execute a command over SSH, if given a remote host.
ssh :: Host -> (NonEmpty Text -> App ()) -> NonEmpty Text -> App ()
ssh Localhost c t = c t
ssh host c t = withSystemTempDir "trilby-update" $ \tmpDir ->
    c . sconcat $
        [ ["ssh"]
        , ["-o", "ControlMaster=auto"]
        , ["-o", "ControlPath=" <> fromPath tmpDir <> "/ssh-%n"]
        , ["-o", "ControlPersist=60"]
        , ["-t"]
        , [ishow host]
        , t
        ]
