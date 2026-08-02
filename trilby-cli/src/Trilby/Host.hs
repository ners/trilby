module Trilby.Host where

import Data.Char (isSpace, toLower)
import Data.List.Extra (split)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static qualified as Reader
import GHC.Generics (Generic)
import Options.Applicative
import Options.Applicative.NonEmpty (some1)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (..))
import Trilby.Util (ishow)
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
    ssh host c t = c $ ["ssh", "-t", ishow host] <> t

parseHosts :: Mod ArgumentFields Host -> Parser (NonEmpty Host)
parseHosts mods = fmap NonEmpty.nubOrd . some1 $ argument auto (metavar "HOST" <> mods)

askHosts :: Maybe (NonEmpty Host) -> Eff es (NonEmpty Host)
askHosts = fmap NonEmpty.nubOrd . maybe (pure [Localhost]) pure
