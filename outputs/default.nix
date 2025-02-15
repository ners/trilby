inputs:

with builtins;
let
  lib = import ../lib { inherit inputs; inherit (inputs.nixpkgs) lib; };
  buildPlatforms = attrNames inputs.nixpkgs.legacyPackages;
in
lib.recursiveConcat [
  {
    inherit lib;
    nixosModules = lib.trilbyModules;
    darwinModules = lib.trilbyModules;
    overlays.default = lib.composeManyExtensions (
      let overlays = attrValues lib.trilbyModules.overlays;
      in map (o: import o { inherit inputs lib overlays; }) overlays
    );
  }
  (lib.foreach buildPlatforms (buildPlatform:
    let
      pkgs = lib.pkgsFor {
        inherit buildPlatform;
        hostPlatform = buildPlatform;
      };
      allConfigs = import ./allConfigs.nix {
        inherit pkgs lib buildPlatform;
      };
    in
    {
      formatter.${buildPlatform} = pkgs.nixpkgs-fmt;
      legacyPackages.${buildPlatform} = pkgs;
      packages.${buildPlatform} = allConfigs // { default = pkgs.trilby-cli; };
    }
  ))
]
