{-# OPTIONS_GHC -Wno-orphans #-}

module Trilby.Install.Flake where

import Data.Text qualified as Text
import Trilby.HNix
import Prelude

data InputOverride = Follows Text Text
    deriving stock (Generic)

instance ToExpr InputOverride where
    toExpr (Follows _ t) = [nix| { follows = t; } |]

data Input = Input
    { name :: Text
    , url :: Text
    , flake :: Bool
    , inputs :: [InputOverride]
    }
    deriving stock (Generic)

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

data NixConfig = NixConfig
    { extraSubstituters :: [Text]
    , extraTrustedPublicKeys :: [Text]
    }
    deriving stock (Generic)

instance ToExpr NixConfig where
    toExpr NixConfig{..} =
        canonicalSet
            [nix|
            {
                extra-substituters = extraSubstituters;
                extra-trusted-public-keys = extraTrustedPublicKeys;
            }
            |]

data Flake = Flake
    { nixConfig :: NixConfig
    , inputs :: [Input]
    , outputs :: NExpr
    }
    deriving stock (Generic)

instance ToExpr Flake where
    toExpr Flake{..} =
        canonicalSet
            [nix|
            {
                nixConfig = nixConfig;
                inputs = inputsSet;
                outputs = outputs;
            }
            |]
      where
        inputsSet = listToSet (fromText . (.name)) inputs

instance Default Flake where
    def =
        Flake
            { nixConfig =
                NixConfig
                    { extraSubstituters = ["https://cache.ners.ch/trilby"]
                    , extraTrustedPublicKeys = ["trilby:AKUGezHi4YbPHCaCf2+XnwWibugjHOwGjH78WqRUnzU="]
                    }
            , inputs =
                [ Input
                    { name = "trilby"
                    , url = "github:ners/trilby"
                    , flake = True
                    , inputs = []
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
