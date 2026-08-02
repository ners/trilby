module Trilby.Host where

import Control.Lens.Operators
import Control.Monad
import Control.Monad.Extra
import Data.Generics.Labels ()
import Data.List.Extra (split)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Semigroup (sconcat)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent.STM (Concurrent)
import Effectful.Fail (Fail)
import Effectful.Reader.Static (Reader, runReader)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import Trilby.App
import Trilby.App ()
import Trilby.Log
import Trilby.Prelude (asRoot, cmdOutTextFirstLine)
import Trilby.Process
import Trilby.System
import Trilby.Util
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

hostname
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Host
    -> Eff es Text
hostname Localhost =
    cached
        ( maybe (fail "hostname failed") pure
            . (listToMaybe . Text.split (== '.') =<<)
            <=< cmdOutTextFirstLine
        )
        ["hostname"]
hostname Host{..} = pure hostname

canonicalHost
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Host
    -> Eff es Host
canonicalHost host = do
    localhostHostnames <- hostname Localhost <&> (: ["localhost", "127.0.0.1", "::1"])
    isLocalhost <- hostname host <&> (`elem` localhostHostnames)
    pure $ if isLocalhost then Localhost else host

onHost :: Host -> Eff (Reader Host ': es) a -> Eff es a
onHost = runReader

-- | Execute a command over SSH, if given a remote host.
ssh :: Host -> (NonEmpty Text -> Eff es a) -> NonEmpty Text -> Eff es a
ssh Localhost c t = c t
ssh host c t = c $ ["ssh", "-t", ishow host] <> t

reboot
    :: (HasCallStack, IOE :> es, TypedProcess :> es, Log :> es)
    => Eff es Bool
    -> Host
    -> Eff es ()
reboot r host = whenM r $ ssh host (asRoot $ runProcess_ . proc) ["systemctl", "reboot"]

hostSystem
    :: ( HasCallStack
       , Fail :> es
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Host
    -> Eff es System
hostSystem host = do
    systemText <-
        cached (maybe (fail "hostSystem failed") pure <=< ssh host cmdOutTextFirstLine)
            . sconcat
            $ [ ["nix", "eval"]
              , ["--impure"]
              , ["--raw"]
              , ["--expr", "builtins.currentSystem"]
              ]
    pure . read . Text.unpack $ systemText

parseHosts :: Mod ArgumentFields String -> Parser (NonEmpty Host)
parseHosts mods = some1 $ fromString <$> strArgument (metavar "HOST" <> mods)

askHosts :: Maybe (NonEmpty Host) -> Eff es (NonEmpty Host)
askHosts = maybe (pure [Localhost]) pure
