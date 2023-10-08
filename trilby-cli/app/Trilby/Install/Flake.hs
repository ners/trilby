{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}

module Trilby.Install.Flake where

import Data.Fix (Fix (Fix))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GHC.Generics (Generic)
import Nix
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.HNix
import Trilby.Util
import Prelude

data InputOverride = Follows Text Text
    deriving stock (Generic, Show, Eq)

data Input = Input
    { name :: Text
    , url :: Text
    , flake :: Bool
    , overrides :: [InputOverride]
    }
    deriving stock (Generic, Show, Eq)

inputBinding :: Input -> Binding NExpr
inputBinding Input{..} =
    canonicalSetBinding
        $ fromText name
        ~:: NSet
            NonRecursive
            ( catMaybes
                [ Just $ "url" ~: fromText url
                , if flake then Nothing else Just $ "flake" ~: toExpr False
                , if null overrides then Nothing else Just overridesSet
                ]
            )
  where
    overridesSet = canonicalSetBinding $ "inputs" ~:: NSet NonRecursive (io <$> overrides)
    io :: InputOverride -> Binding NExpr
    io (Follows s t) = fromText s ~: fromText t

data Flake = Flake
    { inputs :: [Input]
    , outputs :: NExpr
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Flake where
    toExpr Flake{..} =
        [nix|
        {
            inputs = inputsSet;
            outputs = outputs;
        }
        |]
      where
        inputsSet = Fix $ NSet NonRecursive $ inputBinding <$> inputs

trilbyFlake :: Flake
trilbyFlake =
    Flake
        { inputs =
            [ Input
                { name = "trilby"
                , url = "github:ners/trilby"
                , flake = True
                , overrides = []
                }
            ]
        , outputs =
            [nix|
            inputs:
                let lib = inputs.trilby.lib; in
                {
                  nixosConfigurations = with lib; pipe ./hosts [
                    findModules
                    (mapAttrs (_: host: importStmt host { inherit lib; }))
                  ];
                }
        |]
        }
  where
    importStmt = "import" :: NExpr
    pipe = "pipe" :: NExpr
    mapAttrs = "mapAttrs" :: NExpr
    findModules = "findModules" :: NExpr
