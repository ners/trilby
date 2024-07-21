module Trilby.Install.Flake where

import Data.Text qualified as Text
import Trilby.HNix hiding (Flake)
import Trilby.Install.Config.Release (Release (..))
import Prelude

data InputOverride = Follows Text Text
    deriving stock (Generic)

instance ToExpr InputOverride where
    toExpr (Follows _ t) = [nix| { follows = t; } |]

data Input = InputFlake
    { name :: Text
    , url :: Text
    , inputs :: [InputOverride]
    }
    deriving stock (Generic)

instance ToExpr Input where
    toExpr InputFlake{..} =
        [nix|
        {
            url = url;
            inputs = inputsSet;
        }
        |]
      where
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

flake :: Release -> Flake
flake c =
    Flake
        { nixConfig =
            NixConfig
                { extraSubstituters = ["https://cache.ners.ch/trilby"]
                , extraTrustedPublicKeys = ["trilby:AKUGezHi4YbPHCaCf2+XnwWibugjHOwGjH78WqRUnzU="]
                }
        , inputs =
            mconcat
                [ [ InputFlake
                    { name = "nixpkgs-unstable"
                    , url = "github:nixos/nixpkgs/nixos-unstable"
                    , inputs = []
                    }
                  | c /= Unstable
                  ]
                ,
                    [ InputFlake
                        { name = "nixpkgs"
                        , url = "github:nixos/nixpkgs/nixos-" <> ishow c
                        , inputs = []
                        }
                    , InputFlake
                        { name = "trilby"
                        , url = "github:ners/trilby"
                        , inputs =
                            [ "nixpkgs" `Follows` "nixpkgs"
                            , "nixpkgs-unstable"
                                `Follows` if c == Unstable
                                    then "nixpkgs"
                                    else "nixpkgs-unstable"
                            ]
                        }
                    ]
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
