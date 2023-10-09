module Trilby.Config.Channel where

import Data.Default (Default (def))
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr))
import Prelude

data Channel
    = Unstable
    | Stable_23_11
    | Stable_23_05
    deriving stock (Generic, Eq, Bounded, Enum)

instance Show Channel where
    show Unstable = "unstable"
    show Stable_23_11 = "23.11"
    show Stable_23_05 = "23.05"

instance ToExpr Channel where
    toExpr = toExpr . show

instance Default Channel where def = Unstable
