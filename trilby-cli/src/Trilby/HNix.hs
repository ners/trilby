{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Trilby.HNix where

import Data.Fix
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Lens.Family.TH (makeTraversals)
import Nix
import Nix.Atoms (NAtom (NNull))
import Trilby.Host
import Turtle qualified
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

nixBuild :: (HasCallStack) => FileOrFlake -> (FilePath -> App (Path Abs t)) -> App (Path Abs t)
nixBuild f p =
    (p . fromText . firstLine)
        =<< withTrace
            cmd
            ( sconcat
                [ ["nix", "build"]
                , ["--no-link", "--print-out-paths"]
                , case f of
                    File f -> ["--file", fromSomeBase f]
                    Flake f -> ["--accept-flake-config", ishow f]
                ]
            )

copyClosure :: (HasCallStack) => Host -> Path Abs Dir -> App ()
copyClosure Localhost _ = pure ()
copyClosure host@Host{} path = do
    (code, _) <- ssh host cmd' ["command", "-v", "nix-store"]
    case code of
        ExitSuccess ->
            cmd_ . sconcat $
                [ ["nix-copy-closure"]
                , ["--use-substitutes"]
                , ["--gzip"]
                , ["--to", ishow host]
                , [fromPath path]
                ]
        _ -> do
            flags <- sshFlags
            ssh host rawCmd_ ["sudo mkdir -p /nix && sudo chown -R $(whoami) /nix"]
            flip shell_ Turtle.stdin . Text.intercalate " " . sconcat $
                [
                    [ "nix-store"
                    , "--query"
                    , "--requisites"
                    , fromPath path
                    ]
                , ["|"]
                ,
                    [ "tar"
                    , "--create"
                    , "--gzip"
                    , "--file=-"
                    , "--files-from=-"
                    , "--mode='u+rw'"
                    ]
                , ["|"]
                , sconcat
                    [ "ssh" : NonEmpty.toList flags <> [ishow host]
                    ,
                        [ "tar"
                        , "--extract"
                        , "--gzip"
                        , "--file=-"
                        , "--directory=/"
                        ]
                    ]
                ]

trilbyFlake :: (HasCallStack) => App Text
trilbyFlake = do
    hasTrilby <- (ExitSuccess ==) . fst <$> quietCmd' ["nix", "flake", "metadata", "trilby"]
    pure $ if False then "trilby" else "github:ners/trilby/infect"
