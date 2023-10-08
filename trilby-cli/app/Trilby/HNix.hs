{-# OPTIONS_GHC -Wno-orphans #-}

module Trilby.HNix where

import Data.Fix
import Data.List.Extra qualified as List
import Data.String (IsString (fromString))
import Nix
import Text.Megaparsec.Pos (Pos)
import Trilby.Util (fromListSafe)
import Prelude

fakePos :: Pos
fakePos = mkPos 1

fakeSourcePos :: SourcePos
fakeSourcePos = SourcePos "<foo>" fakePos fakePos

fakeSrcSpan :: SrcSpan
fakeSrcSpan = SrcSpan fakeSourcePos fakeSourcePos

instance IsString (NAttrPath NExpr) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

instance IsString (NAttrPath NExprLoc) where
    fromString = fromListSafe "" . fmap (StaticKey . fromString) . List.splitOn "."

infixl 4 ~:

(~:) :: NAttrPath r -> r -> Binding r
k ~: v = NamedVar k v fakeSourcePos

infixl 4 ~::

(~::) :: NAttrPath (Fix f) -> f (Fix f) -> Binding (Fix f)
k ~:: v = k ~: Fix v

canonicalSetBinding :: Binding NExpr -> Binding NExpr
canonicalSetBinding (NamedVar p1 (Fix (NSet _ [NamedVar p2 e _])) pos) = NamedVar (p1 <> p2) e pos
canonicalSetBinding b = b
