module Trilby.Config.Cache where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAlphaNum)
import Data.Text qualified as Text
import Effectful.FileSystem qualified as FileSystem
import Effectful.FileSystem.Path.IO (XdgDirectory (XdgCache))
import Trilby.Prelude

data Refresh
    = UseCache
    | Refresh
    deriving stock (Eq)

data Cache = Cache
    { refresh :: Refresh
    , self :: Text
    }

encode :: Text -> Value -> ByteString
encode key value = LazyByteString.toStrict $ Aeson.encode (key, value)

decode :: Text -> ByteString -> Maybe Value
decode key bytes = do
    (stored, value) <- Aeson.decodeStrict bytes
    guard $ stored == key
    pure value

fileName :: Text -> FilePath
fileName = Text.unpack . Text.map \c -> if isAlphaNum c || c == '.' || c == '-' || c == '+' then c else '_'

location :: (FileSystem :> es) => Text -> Eff es (Path Abs Dir, FilePath)
location label = do
    directory <- getXdgDir XdgCache $ Just $(mkRelDir "trilby/config")
    pure (directory, toFilePath directory <> fileName label)

fetch :: (IOE :> es, FileSystem :> es) => Cache -> Text -> Text -> Eff es (Maybe Value)
fetch Cache{refresh = Refresh} _ _ = pure Nothing
fetch Cache{refresh = UseCache} label key = do
    (_, file) <- location label
    handle (const @_ @SomeException $ pure Nothing) $ decode key <$> liftIO (ByteString.readFile file)

store :: (IOE :> es, FileSystem :> es) => Text -> Text -> Value -> Eff es ()
store label key value = do
    (directory, file) <- location label
    handle (const @_ @SomeException $ pure ()) do
        FileSystem.createDirectoryIfMissing True $ toFilePath directory
        liftIO . ByteString.writeFile file $ encode key value
