module Trilby.System where

import Data.List.Extra qualified as List
import Data.Text qualified as Text
import Text.Read (readMaybe)
import Prelude

newtype Architecture = Architecture Text
    deriving newtype (Eq, Ord)

instance Show Architecture where
    show (Architecture a) = Text.unpack a

data Kernel
    = Linux
    | Darwin
    deriving stock (Generic, Eq, Ord, Show, Bounded, Enum)

instance Read Kernel where
    readsPrec = readsPrecBoundedEnumOn (fmap toLower)

data System = System
    { architecture :: Architecture
    , kernel :: Kernel
    }
    deriving stock (Generic, Eq, Ord)

instance Show System where
    show System{..} = show architecture <> "-" <> fmap toLower (show kernel)

instance Read System where
    readsPrec :: Int -> String -> [(System, String)]
    readsPrec _ s = maybeToList do
        (archPart, kernelPart) <-
            case List.splitOn "-" s of
                [archPart, kernelPart] -> Just (archPart, kernelPart)
                _ -> Nothing
        let architecture = Architecture $ Text.pack archPart
        kernel <- readMaybe kernelPart
        pure (System{..}, "")
