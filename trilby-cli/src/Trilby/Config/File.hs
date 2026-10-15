module Trilby.Config.File where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Trilby.Config.Edit (Error (..))
import Trilby.Config.Effects (Configuring)
import Trilby.HNix (writeNixFile)
import Trilby.Prelude

tryModify
    :: (Configuring es)
    => Path Abs File
    -> (Text -> Either Error Text)
    -> Eff es (Either Error ())
tryModify file edit = do
    text <- liftIO . Text.readFile $ fromPath file
    for (edit text) \new -> when (new /= text) $ writeFile file new

modify :: (Configuring es) => Path Abs File -> (Text -> Either Error Text) -> Eff es ()
modify file edit = tryModify file edit >>= either (errorExit . describe file) pure

describe :: Path Abs File -> Error -> Text
describe file NotFound = fromPath file <> ": nothing to change"
describe file (Failed reason) = fromPath file <> ": " <> Text.pack reason

create :: (Configuring es, ToExpr a) => Path Abs File -> a -> Eff es ()
create file expression = do
    writeNixFile file expression
    logAttention_ $ "Created " <> fromPath file <> ", make sure that Nix can see it"
