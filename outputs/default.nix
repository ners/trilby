inputs:

with builtins;
let
  lib = import ../lib { inherit inputs; inherit (inputs.nixpkgs) lib; };
  buildPlatforms = attrNames inputs.nixpkgs.legacyPackages;
  nixosModules = lib.findModules ../modules;
in
lib.recursiveConcat [
  {
    inherit lib nixosModules;
    overlays.default = lib.composeManyExtensions (
      let overlays = attrValues nixosModules.overlays;
      in map (o: import o { inherit inputs lib overlays; }) overlays
    );
  }
  (lib.foreach buildPlatforms (buildPlatform:
    let
      pkgs = lib.pkgsFor {
        inherit buildPlatform;
        hostPlatform = buildPlatform;
      };
    in
    {
      formatter.${buildPlatform} = pkgs.nixpkgs-fmt;
      legacyPackages.${buildPlatform} = pkgs;
      packages.${buildPlatform} = import ./allConfigs.nix { inherit pkgs lib nixosModules buildPlatform; } // {
        default = pkgs.trilby-cli;
      };
    }
  ))
]
