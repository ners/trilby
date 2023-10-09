{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Trilby.HNix where

import Data.Fix
import Data.List.Extra qualified as List
import Data.String (IsString (fromString))
import Lens.Family.TH (makeTraversals)
import Nix
import Nix.Atoms (NAtom (NNull))
import Nix.TH (ToExpr (toExpr))
import Trilby.Util (fromListSafe)
import Prelude

instance IsString (NAttrPath NExpr) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

instance IsString (NAttrPath NExprLoc) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

infixl 4 ~:

(~:) :: NAttrPath r -> r -> Binding r
k ~: v = NamedVar k v fakeSourcePos
  where
    fakePos = mkPos 1
    fakeSourcePos = SourcePos "<foo>" fakePos fakePos

infixl 4 ~::

(~::) :: NAttrPath (Fix f) -> f (Fix f) -> Binding (Fix f)
k ~:: v = k ~: Fix v

$(makeTraversals ''Fix)

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
