{-# OPTIONS_GHC -Wno-orphans #-}

module Trilby.HNix where

import Data.Fix
import Data.List.Extra qualified as List
import Data.String (IsString (fromString))
import Debug.Trace (trace)
import Nix
import Text.Megaparsec.Pos (Pos)
import Trilby.Util
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

unAnnotate :: NExprLoc -> NExprF a
unAnnotate (Fix (Compose (AnnUnit _ e))) = foo e

foo :: NExprF (Fix NExprLocF) -> NExprF r
foo (NConstant atom) = NConstant atom
foo (NStr str) = trace (show str) undefined
foo (NSym varName) = NSym varName
foo (NList xs) = NList $ trace (show xs) undefined
foo (NSet recursivity bindings) = NSet recursivity $ b <$> bindings
  where
    b :: Binding (Fix NExprLocF) -> Binding r
    b (NamedVar path r pos) =
        NamedVar
            (trace (show path) undefined)
            (trace (show r) undefined)
            pos
    b (Inherit r vars pos) =
        Inherit
            (trace (show r) undefined)
            vars
            pos
foo (NLiteralPath path) = NLiteralPath path
foo (NEnvPath path) = NEnvPath path
foo (NUnary op x) = NUnary op $ trace (show x) undefined
foo (NBinary op x y) =
    NBinary
        op
        (trace (show x) undefined)
        (trace (show y) undefined)
foo e = trace (show e) undefined

unAnnotateF :: NExprLocF a -> NExprF a
unAnnotateF (AnnF _ e) = e
