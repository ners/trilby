module Trilby.Config.Hosts where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Effects (Configuring)
import Trilby.Config.Eval (Inventory (..))
import Trilby.Config.Eval qualified as Eval
import Trilby.Config.Target qualified as Target
import Trilby.HNix (writeNixFile)
import Trilby.Install.Config.Edition (Edition)
import Trilby.Install.Config.Host (systemModule)
import Trilby.Install.Options (validateHostname)
import Trilby.Prelude
import Trilby.Widgets (selectEnum)

bareHost :: Text -> NExpr
bareHost hostname = [nix| { ... }: { networking.hostName = hostname; } |]

list :: (Configuring es) => Path Abs Dir -> Eff es ()
list flake = Eval.inventory flake >>= liftIO . Text.putStr . Text.unlines . (.hosts)

add :: (Configuring es) => Path Abs Dir -> Text -> Maybe Edition -> Maybe Text -> Eff es ()
add flake hostname edition platform = do
    either (errorExit . fromString) (const $ pure ()) $ validateHostname hostname
    inventory <- Eval.inventory flake
    when (hostname `elem` inventory.hosts) . errorExit $ "Host already exists: " <> hostname
    chosenEdition <- maybe (selectEnum "Choose edition:" Nothing) pure edition
    chosenPlatform <- maybe (show <$> hostSystem) (pure . Text.unpack) platform
    systemFile <- Target.relativeFile flake $ "hosts/" <> hostname <> "/default.nix"
    hostFile <- Target.relativeFile flake $ "hosts/" <> hostname <> "/configuration.nix"
    writeNixFile systemFile $ systemModule chosenEdition chosenPlatform
    writeNixFile hostFile $ bareHost hostname
    logAttention_ $ "Created " <> fromPath (parent hostFile) <> ", make sure that Nix can see it"
