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

class UnAnnotate ann plain where
    unAnnotate :: ann -> plain

instance {-# OVERLAPPABLE #-} UnAnnotate x x where
    unAnnotate = id

instance (UnAnnotate s s', UnAnnotate r r') => UnAnnotate (Antiquoted s r) (Antiquoted s' r') where
    unAnnotate EscapedNewline = EscapedNewline
    unAnnotate (Plain x) = Plain (unAnnotate x)
    unAnnotate (Antiquoted x) = Antiquoted (unAnnotate x)

instance (UnAnnotate ann plain) => UnAnnotate (NString ann) (NString plain) where
    unAnnotate (DoubleQuoted xs) = DoubleQuoted $ unAnnotate <$> xs
    unAnnotate (Indented k xs) = Indented k $ unAnnotate <$> xs

instance (UnAnnotate ann plain) => UnAnnotate (NKeyName ann) (NKeyName plain) where
    unAnnotate (DynamicKey x) = DynamicKey (unAnnotate x)
    unAnnotate (StaticKey x) = StaticKey x

instance (UnAnnotate ann plain) => UnAnnotate (NAttrPath ann) (NAttrPath plain) where
    unAnnotate = fmap unAnnotate

instance (UnAnnotate ann plain) => UnAnnotate (Binding ann) (Binding plain) where
    unAnnotate (NamedVar path r pos) = NamedVar (unAnnotate path) (unAnnotate r) pos
    unAnnotate (Inherit r vars pos) = Inherit (unAnnotate <$> r) vars pos

instance UnAnnotate NExprLoc NExpr where
    unAnnotate :: NExprLoc -> NExpr
    unAnnotate (Fix (Compose (AnnUnit _ e))) = Fix $ case e of
        (NConstant atom) -> NConstant atom
        (NStr str) -> NStr $ unAnnotate str
        (NSym varName) -> NSym varName
        (NList xs) -> NList $ fmap unAnnotate xs
        (NSet recursivity bindings) -> NSet recursivity $ unAnnotate <$> bindings
        (NLiteralPath path) -> NLiteralPath path
        (NEnvPath path) -> NEnvPath path
        (NUnary op x) -> NUnary op (unAnnotate x)
        (NBinary op x y) -> NBinary op (unAnnotate x) (unAnnotate y)
        _ -> trace (show e) undefined
