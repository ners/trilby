module Trilby.Disko where

import Internal.Prelude
import Nix.Prelude (One (one))
import Trilby.Disko.Disk
import Trilby.HNix

newtype Disko = Disko {disks :: [Disk]}
    deriving stock (Generic)

instance ToExpr Disko where
    toExpr Disko{..} =
        [nix|
        {
            disko.devices.disk = diskSet;
        }
        |]
      where
        diskSet = listToSet (one . DynamicKey . Plain . fromText . (.device)) disks
