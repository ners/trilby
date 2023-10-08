module Trilby.Config.Edition where

import Data.Char (toLower)
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr))
import Prelude

data Edition = Workstation | Server
    deriving stock (Generic, Show, Read, Eq)

instance ToExpr Edition where
    toExpr = toExpr . fmap toLower . show
