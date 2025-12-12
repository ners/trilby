{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Trilby.HNix where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Fix
import Data.List.Extra qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lens.Family.TH (makeTraversals)
import Nix
import Nix.Atoms (NAtom (NNull))
import Trilby.Host
import Trilby.Prelude
import Trilby.Process

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

showNix :: (ToExpr e, IsString s) => e -> s
showNix = fromString . show . prettyNix . toExpr

writeNixFile :: (ToExpr a) => Path b File -> a -> App ()
writeNixFile f = writeFile f . showNix

data FileOrFlake
    = File (SomeBase File)
    | Flake FlakeRef
    deriving stock (Generic)

nixBuild :: (HasCallStack) => FileOrFlake -> App [Path Abs Dir]
nixBuild f =
    mapM (parseAbsDir . fromText)
        . Text.lines
        . Text.decodeUtf8
        . LazyByteString.toStrict
        =<< withTrace
            (readProcessStdout_ . proc)
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
    ssh host cmdCode ["command", "-v", "nix-store"] >>= \case
        ExitSuccess ->
            cmd_
                . sconcat
                $ [ ["nix-copy-closure"]
                  , ["--use-substitutes"]
                  , ["--gzip"]
                  , ["--to", ishow host]
                  , [fromPath path]
                  ]
        _ -> do
            ssh host (asRoot $ runProcess_ . proc) ["sh", "-c", "mkdir -p /nix && chown -R $(whoami) /nix"]
            shell_
                . sconcat
                $ [
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
                        [ "ssh" :| ["-t", ishow host]
                        ,
                            [ "tar"
                            , "--extract"
                            , "--gzip"
                            , "--file=-"
                            , "--directory=/"
                            ]
                        ]
                  ]

trilbyFlake :: (HasCallStack) => [Text] -> App FlakeRef
trilbyFlake output = do
    url <- fromMaybeM (pure fallbackUrl) $ firstJustM id [urlFromEnv, urlFromRegistry]
    pure FlakeRef{..}
  where
    urlFromEnv, urlFromRegistry :: App (Maybe Text)
    urlFromEnv = fromString <$$> lookupEnv "TRILBY"
    urlFromRegistry =
        cached cmdCode ["nix", "flake", "metadata", "trilby"] <&> \case
            ExitSuccess -> Just "trilby"
            _ -> Nothing
    fallbackUrl :: Text
    fallbackUrl = "github:ners/trilby"
