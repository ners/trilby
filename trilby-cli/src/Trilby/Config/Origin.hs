module Trilby.Config.Origin where

import Data.Text qualified as Text
import Trilby.Prelude

data Origin
    = Local
    | Trilby
    | Nixos
    | Input Text
    deriving stock (Eq, Ord)

instance Show Origin where
    show Local = "local"
    show Trilby = "trilby"
    show Nixos = "nixos"
    show (Input name) = Text.unpack name

instance IsString Origin where
    fromString "local" = Local
    fromString "trilby" = Trilby
    fromString "nixos" = Nixos
    fromString name = Input $ fromString name

type Roots = [(Text, Origin)]

locate :: Roots -> Text -> (Origin, Text)
locate roots file =
    fromMaybe (Input "unknown", file)
        $ asum [(origin,) <$> Text.stripPrefix (root <> "/") file | (root, origin) <- roots]
