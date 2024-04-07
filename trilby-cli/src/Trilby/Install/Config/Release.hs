module Trilby.Install.Config.Release where

import Prelude

data Release
    = Unstable
    | Stable_23_11
    | Stable_23_05
    deriving stock (Generic, Eq, Bounded, Enum)

instance Show Release where
    show Unstable = "unstable"
    show Stable_23_11 = "23.11"
    show Stable_23_05 = "23.05"

instance Read Release where
    readsPrec = readsPrecBoundedEnum

instance ToExpr Release where
    toExpr = toExpr . show

instance Default Release where def = Unstable
