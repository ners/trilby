module Trilby.Install.Config.Release where

import Trilby.Prelude

data Release
    = Unstable
    | Stable_25_11
    | Stable_25_05
    | Stable_24_11
    deriving stock (Generic, Eq, Bounded, Enum)

instance Show Release where
    show Unstable = "unstable"
    show Stable_25_11 = "25.11"
    show Stable_25_05 = "25.05"
    show Stable_24_11 = "24.11"

instance Read Release where
    readPrec = readPrecBoundedEnum

instance ToExpr Release where
    toExpr = toExpr . fmap (\c -> if c == '.' then '_' else c) . show

instance Default Release where def = Unstable
