{-# OPTIONS_GHC -Wno-orphans #-}

module Trilby.Install.Flake where

import Data.Default (Default (def))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Nix.TH (ToExpr (toExpr), nix)
import Trilby.HNix
import Trilby.Util
import Prelude
import Nix (NExpr, NAttrPath)

data InputOverride = Follows Text Text
    deriving stock (Generic, Show, Eq)

instance ToExpr InputOverride where
    toExpr (Follows _ t) = [nix| { follows = t; } |]

data Input = Input
    { name :: Text
    , url :: Text
    , flake :: Bool
    , inputs :: [InputOverride]
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Input where
    toExpr Input{..} =
        [nix|
        {
            url = url;
            flake = flakeBinding;
            inputs = inputsSet;
        }
        |]
      where
        flakeBinding = if flake then Nothing else Just False
        inputsSet = listToSet io inputs
        io (Follows s _) = fromText @(NAttrPath NExpr) $ if Text.elem '.' s then doubleQuoted s else s

data Flake = Flake
    { inputs :: [Input]
    , outputs :: NExpr
    }
    deriving stock (Generic, Show, Eq)

instance ToExpr Flake where
    toExpr Flake{..} =
        canonicalSet
            [nix|
            {
                inputs = inputsSet;
                outputs = outputs;
            }
            |]
      where
        inputsSet = listToSet (fromText . (.name)) inputs

instance Default Flake where
    def =
        Flake
            { inputs =
                [ Input
                    { name = "nixpkgs"
                    , url = "github:nixos/nixpkgs/nixos-unstable"
                    , flake = True
                    , inputs = []
                    }
                , Input
                    { name = "nixpkgs-23_05"
                    , url = "github:nixos/nixpkgs/nixos-23.05"
                    , flake = True
                    , inputs = []
                    }
                , Input
                    { name = "trilby"
                    , url = "github:ners/trilby"
                    , flake = True
                    , inputs =
                        [ Follows "nixpkgs-unstable" "nixpkgs"
                        , Follows "nixpkgs-23.05" "nixpkgs-23_05"
                        ]
                    }
                ]
            , outputs =
                [nix|
                inputs:
                with builtins;
                let lib = inputs.trilby.lib; in
                {
                  nixosConfigurations = with lib; pipe ./hosts [
                    findModules
                    (mapAttrs (_: host: import host { inherit lib; }))
                  ];
                }
                |]
            }
