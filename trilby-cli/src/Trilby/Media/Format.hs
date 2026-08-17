module Trilby.Media.Format where

import Trilby.Prelude

data Format = IsoImage
    deriving stock (Bounded, Enum, Eq, Ord)

instance Show Format where
    show IsoImage = "iso-image"

instance Read Format where
    readPrec = readPrecBoundedEnum

instance ToExpr Format where
    toExpr IsoImage = toExpr @String "isoImage"
