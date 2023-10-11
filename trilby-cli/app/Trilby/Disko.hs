module Trilby.Disko where

import GHC.Generics (Generic)
import Nix
import Nix.Prelude (One (one))
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.Disko.Disk
import Trilby.HNix
import Trilby.Util (fromText)
import Prelude

newtype Disko = Disko {disks :: [Disk]}
    deriving stock (Generic, Show, Eq)

instance ToExpr Disko where
    toExpr Disko{..} =
        [nix|
        {
            disko.devices.disk = diskSet;
        }
        |]
      where
        diskSet = listToSet (one . DynamicKey . Plain . fromText . (.device)) disks
