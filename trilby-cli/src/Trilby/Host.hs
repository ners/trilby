module Trilby.Host where

import Control.Exception (IOException)
import Control.Exception qualified as Exception
import Control.Monad (guard, replicateM)
import Data.Bifunctor (second)
import Data.Char (isSpace, toLower)
import Data.List.Extra (split)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import Network.Socket qualified as Socket
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (..))
import Trilby.Util (ishow, singleQuoted)
import Prelude

data Host
    = Localhost
    | Host {username :: Maybe Text, hostname :: Text}
    deriving stock (Generic, Eq, Ord)

instance IsString Host where
    fromString (fmap toLower -> "localhost") = Localhost
    fromString (split (== '@') -> (fromString -> Just -> username) : (fromString -> hostname) : _) = Host{..}
    fromString s = Host{username = Nothing, hostname = fromString s}

instance Show Host where
    show Localhost = "localhost"
    show Host{username = Nothing, ..} = Text.unpack hostname
    show Host{username = Just username, ..} = Text.unpack $ username <> "@" <> hostname

instance Read Host where
    readPrec = fromString <$> ReadPrec.lift (ReadP.munch1 $ not . isSpace)

-- | Run an action with a specific Host in scope. Command runners in "Trilby.Prelude"
-- automatically execute over SSH via 'ssh' whenever the Host in scope is not 'Localhost'.
onHost :: Host -> Eff (Reader Host ': es) a -> Eff es a
onHost = runReader

withHost :: (Reader Host :> es) => (NonEmpty Text -> Eff es a) -> NonEmpty Text -> Eff es a
withHost c t = Reader.ask >>= \host -> ssh host c t
  where
    ssh :: Host -> (NonEmpty Text -> Eff es a) -> NonEmpty Text -> Eff es a
    ssh Localhost c t = c t
    ssh host c t = c $ ["ssh", "-t", ishow host] <> (singleQuoted <$> t)

parseHosts :: Mod ArgumentFields Host -> Parser (NonEmpty Host)
parseHosts mods = fmap NonEmpty.nubOrd . some1 $ argument auto (metavar "HOST" <> mods)

askHosts :: Maybe (NonEmpty Host) -> Eff es (NonEmpty Host)
askHosts = fmap NonEmpty.nubOrd . maybe (pure [Localhost]) pure

-- | The hostname and port from @ssh -G@ output, unless the connection goes through a proxy.
sshTarget :: Text -> Maybe (Text, Text)
sshTarget sshConfig = do
    guard $ all @[] unset ["proxyjump", "proxycommand"]
    hostname <- lookup "hostname" entries
    pure (hostname, fromMaybe "22" $ lookup "port" entries)
  where
    entries = second Text.strip . Text.breakOn " " <$> Text.lines sshConfig
    unset key = maybe True (== "none") $ lookup key entries

-- | Shortest of three TCP connect times in seconds.
-- Nothing if an attempt fails or takes over a second.
connectTime :: Text -> Text -> IO (Maybe Double)
connectTime hostname port = Exception.handle (const @_ @IOException $ pure Nothing) do
    addr : _ <-
        Socket.getAddrInfo
            (Just Socket.defaultHints{Socket.addrSocketType = Socket.Stream})
            (Just $ Text.unpack hostname)
            (Just $ Text.unpack port)
    fmap minimum . sequence <$> replicateM 3 do
        Exception.bracket (Socket.openSocket addr) Socket.close \sock -> do
            start <- getMonotonicTime
            timeout 1_000_000 (Socket.connect sock $ Socket.addrAddress addr)
                >>= traverse (const $ subtract start <$> getMonotonicTime)
