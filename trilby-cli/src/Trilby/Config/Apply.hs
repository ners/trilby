module Trilby.Config.Apply where

import Trilby.Config.Effects (Configuring)
import Trilby.Configuration qualified as Configuration
import Trilby.Prelude
import Trilby.Update (update)
import Trilby.Update.Options (UpdateAction, UpdateOpts (..))

run :: (Configuring es) => Path Abs Dir -> Maybe (UpdateAction Maybe) -> [Text] -> Eff es ()
run flake action hosts = do
    current <- Configuration.hostname
    if flake == trilbyHome rootDir && current `elem` hosts
        then update UpdateOpts{flakeUpdate = Just False, action, hosts = Nothing}
        else logAttention_ "Changes made, run `trilby update` on the affected hosts to apply them"
