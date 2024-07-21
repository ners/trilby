{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Trilby.HNix where

import Data.Fix
import Data.List.Extra qualified as List
import Data.Text qualified as Text
import Lens.Family.TH (makeTraversals)
import Nix
import Nix.Atoms (NAtom (NNull))
import Path.Internal.Posix (Path (Path))
import Trilby.Host
import Prelude

instance IsString (NAttrPath NExpr) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

instance IsString (NAttrPath NExprLoc) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

infixl 4 ~:

(~:) :: NAttrPath r -> r -> Binding r
k ~: v = NamedVar k v fakeSourcePos
  where
    fakeSourcePos = NSourcePos "<foo>" fakePos fakePos
    fakePos = NPos $ mkPos 1

infixl 4 ~::

(~::) :: NAttrPath (Fix f) -> f (Fix f) -> Binding (Fix f)
k ~:: v = k ~: Fix v

$(makeTraversals ''Fix)

instance ToExpr (Path b t) where
    toExpr = toExpr @Text . fromPath

instance ToExpr (SomeBase t) where
    toExpr = toExpr @Text . fromSomeBase

canonicalBinding :: Binding NExpr -> Binding NExpr
canonicalBinding (NamedVar p1 (Fix (NSet _ [NamedVar p2 e _])) pos) = canonicalBinding $ NamedVar (p1 <> p2) e pos
canonicalBinding (NamedVar p1 s@(Fix (NSet{})) pos) = NamedVar p1 (canonicalSet s) pos
canonicalBinding b = b

canonicalSet :: NExpr -> NExpr
canonicalSet (Fix (NSet r bs)) = Fix $ NSet r $ canonicalBinding <$> filter (not . isNullBinding) bs
  where
    isNullBinding :: Binding NExpr -> Bool
    isNullBinding (NamedVar _ (Fix (NConstant NNull)) _) = True
    isNullBinding (NamedVar _ (Fix (NSet _ [])) _) = True
    isNullBinding (NamedVar _ (Fix (NList [])) _) = True
    isNullBinding _ = False
canonicalSet s = error $ "canonicalSet bottom: " <> show s

listToSet :: (ToExpr a) => (a -> NAttrPath NExpr) -> [a] -> NExpr
listToSet f xs = Fix $ NSet NonRecursive $ (\x -> f x ~: toExpr x) <$> xs

data FlakeRef = FlakeRef
    { url :: Text
    , output :: [Text]
    }
    deriving stock (Generic)

instance IsString FlakeRef where
    fromString (fromString -> Text.break (== '#') -> (url, Text.split (== '.') -> output)) =
        FlakeRef{..}

instance Show FlakeRef where
    show FlakeRef{output = [], ..} = Text.unpack url
    show FlakeRef{..} = Text.unpack $ url <> "#" <> Text.intercalate "." output

data FileOrFlake
    = File (SomeBase File)
    | Flake FlakeRef
    deriving stock (Generic)

showNix :: (ToExpr e, IsString s) => e -> s
showNix = fromString . show . prettyNix . toExpr

writeNixFile :: (ToExpr a) => Path b File -> a -> App ()
writeNixFile f = writeFile f . showNix

nixBuild :: (HasCallStack) => FileOrFlake -> App (Path Abs t)
nixBuild f =
    fmap (Path . fromText . firstLine) . withTrace cmd . sconcat $
        [ ["nix", "build"]
        , ["--no-link", "--print-out-paths"]
        , case f of
            File f -> ["--file", fromSomeBase f]
            Flake f -> ["--accept-flake-config", ishow f]
        ]

copyClosure :: (HasCallStack) => Host -> Path Abs Dir -> App ()
copyClosure Localhost _ = pure ()
copyClosure host@Host{} path = cmd_ ["nix-copy-closure", "--gzip", "--to", ishow host, fromPath path]

currentSystem :: (HasCallStack) => App Text
currentSystem =
    fmap firstLine . cmd . sconcat $
        [ ["nix", "eval"]
        , ["--impure"]
        , ["--raw"]
        , ["--expr", "builtins.currentSystem"]
        ]

trilbyFlake :: (HasCallStack) => App Text
trilbyFlake = do
    hasTrilby <- (ExitSuccess ==) . fst <$> quietCmd' ["nix", "flake", "metadata", "trilby"]
    pure $ if False then "trilby" else "github:ners/trilby/infect"
