module Trilby.Install.Flake where

import Data.Text qualified as Text
import Trilby.HNix hiding (Flake)
import Trilby.Install.Config.Release
import Trilby.System
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
        [nix|
        {
            extra-substituters = extraSubstituters;
            extra-trusted-public-keys = extraTrustedPublicKeys;
        }
        |]
            & canonicalSet

data Flake = Flake
    { nixConfig :: NixConfig
    , inputs :: [Input]
    , outputs :: NExpr
    }
    deriving stock (Generic)

instance ToExpr Flake where
    toExpr Flake{..} =
        [nix|
            {
                nixConfig = nixConfig;
                inputs = inputsSet;
                outputs = outputs;
            }
        |]
            & canonicalSet
      where
        inputsSet = listToSet (fromText . (.name)) inputs

flake :: Kernel -> Release -> Flake
flake kernel release =
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
                  | release /= Unstable
                  ]
                , [ InputFlake
                    { name = "nix-darwin"
                    , url = "github:LnL7/nix-darwin"
                    , inputs = ["nixpkgs" `Follows` "nixpkgs"]
                    }
                  | kernel == Darwin
                  ]
                ,
                    [ InputFlake
                        { name = "nixpkgs"
                        , url = "github:nixos/nixpkgs/nixos-" <> ishow release
                        , inputs = []
                        }
                    , InputFlake
                        { name = "home-manager"
                        , url =
                            "github:nix-community/home-manager"
                                <> if release == Unstable
                                    then ""
                                    else "/release-" <> ishow release
                        , inputs = []
                        }
                    , InputFlake
                        { name = "trilby"
                        , url = "github:ners/trilby"
                        , inputs =
                            [ "nixpkgs" `Follows` "nixpkgs"
                            , "nixpkgs-unstable"
                                `Follows` if release == Unstable
                                    then "nixpkgs"
                                    else "nixpkgs-unstable"
                            , "home-manager" `Follows` "home-manager"
                            ]
                                <> ["nix-darwin" `Follows` "nix-darwin" | kernel == Darwin]
                        }
                    ]
                ]
        , outputs =
            [nix|
                inputs:
                with builtins;
                let
                  inherit (inputs.trilby) lib;
                  allConfigurations = with lib; pipe ./hosts [
                    findModules
                    (mapAttrs (hostname: host: import host { inherit inputs lib; }))
                  ];
                in
                {
                  inherit (inputs.trilby) legacyPackages;
                  nixosConfigurations = allConfigurations;
                  darwinConfigurations = allConfigurations;
                }
                |]
        }
