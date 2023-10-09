module Trilby.Config.Edition where

import Data.Char (toLower)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr))
import Prelude

data Edition = Workstation | Server
    deriving stock (Generic, Show, Eq, Bounded, Enum)

instance ToExpr Edition where
    toExpr = toExpr . fmap toLower . show

instance Default Edition where def = Workstation
