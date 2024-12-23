module Trilby.Install.Config.Release where

import Prelude

data Release
    = Unstable
    | Stable_24_11
    | Stable_24_05
    deriving stock (Generic, Eq, Bounded, Enum)

instance Show Release where
    show Unstable = "unstable"
    show Stable_24_11 = "24.11"
    show Stable_24_05 = "24.05"

instance Read Release where
    readPrec = readPrecBoundedEnum

instance ToExpr Release where
    toExpr = toExpr . show

instance Default Release where def = Unstable
