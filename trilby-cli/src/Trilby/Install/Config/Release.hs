module Trilby.Install.Config.Release where

import Trilby.Prelude

data Release
    = Unstable
    | Stable_26_05
    | Stable_25_11
    | Stable_25_05
    deriving stock (Generic, Eq, Bounded, Enum)

instance Show Release where
    show Unstable = "unstable"
    show Stable_26_05 = "26.05"
    show Stable_25_11 = "25.11"
    show Stable_25_05 = "25.05"

instance Read Release where
    readPrec = readPrecBoundedEnum

instance ToExpr Release where
    toExpr = toExpr . show

instance Default Release where def = Unstable
