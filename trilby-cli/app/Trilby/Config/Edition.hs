module Trilby.Config.Edition where

import Data.Char (toLower)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr))
import Trilby.Util
import Prelude

data Edition = Workstation | Server
    deriving stock (Generic, Show, Eq, Bounded, Enum)

instance Read Edition where
    readsPrec = readsPrecBoundedEnumOn (fmap toLower)

instance ToExpr Edition where
    toExpr = toExpr . fmap toLower . show

instance Default Edition where def = Workstation
