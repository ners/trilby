module Trilby.Install.Config.Release where

import Prelude

data Release
    = Unstable
    | Stable_24_05
    | Stable_23_11
    deriving stock (Generic, Eq, Bounded, Enum)

instance Show Release where
    show Unstable = "unstable"
    show Stable_24_05 = "24.05"
    show Stable_23_11 = "23.11"

instance Read Release where
    readsPrec = readsPrecBoundedEnum

instance ToExpr Release where
    toExpr = toExpr . show

instance Default Release where def = Unstable
