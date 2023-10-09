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
canonicalSet (Fix (NSet recursivity bindings)) = Fix $ NSet recursivity $ canonicalBinding <$> filter (not . isNullBinding) bindings
  where
    isNullBinding :: Binding NExpr -> Bool
    isNullBinding (NamedVar _ (Fix (NConstant NNull)) _) = True
    isNullBinding (NamedVar _ (Fix (NSet _ [])) _) = True
    isNullBinding (NamedVar _ (Fix (NList [])) _) = True
    isNullBinding _ = False
canonicalSet s = error $ "canonicalSet bottom: " <> show s

appendBinding :: Binding NExpr -> NExpr -> NExpr
appendBinding b (Fix (NSet recursivity bindings)) = Fix $ NSet recursivity $ bindings <> [b]
appendBinding b (Fix (NBinary o f@(Fix (NSet{})) e)) = Fix $ NBinary o (appendBinding b f) e
appendBinding b (Fix (NBinary o f e@(Fix (NSet{})))) = Fix $ NBinary o f (appendBinding b e)
appendBinding b (Fix (NAbs params e)) = Fix $ NAbs params $ appendBinding b e
appendBinding _ e = error $ "appendBinding bottom: " <> show e
