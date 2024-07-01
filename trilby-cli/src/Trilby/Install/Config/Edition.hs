module Trilby.Install.Config.Edition where

import Prelude

data Edition = Workstation | Server
    deriving stock (Generic, Show, Eq, Bounded, Enum)

instance Read Edition where
    readsPrec = readsPrecBoundedEnumOn (fmap toLower)

instance ToExpr Edition where
    toExpr = toExpr . fmap toLower . show

instance Default Edition where def = Workstation
