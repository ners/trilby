module Trilby.Configuration where

import Data.Text qualified as Text
import Effectful.Reader.Static qualified as Reader
import Trilby.Host
import Trilby.Prelude

data Configuration = Configuration
    { name :: Text
    , host :: Host
    }
    deriving stock (Generic, Eq, Ord, Show)

hostname
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Eff es Text
hostname =
    Reader.ask >>= \case
        Localhost ->
            cached
                ( maybe (errorExit "hostname failed") pure
                    . (listToMaybe . Text.split (== '.') =<<)
                    <=< cmdOutTextFirstLine
                )
                ["hostname"]
        Host{..} -> pure hostname

fromHost
    :: ( HasCallStack
       , IOE :> es
       , Concurrent :> es
       , Reader AppState :> es
       , TypedProcess :> es
       , Log :> es
       )
    => Host
    -> Eff es Configuration
fromHost host = do
    name <- onHost host hostname
    pure Configuration{..}

reboot
    :: ( HasCallStack
       , IOE :> es
       , Reader Host :> es
       , TypedProcess :> es
       , Log :> es
       , Reader AppState :> es
       , Concurrent :> es
       )
    => Eff es ()
reboot = asRoot (withHost $ runProcess_ . proc) ["systemctl", "reboot"]
