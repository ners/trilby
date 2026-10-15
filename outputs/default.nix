inputs:

with builtins;
let
  lib = import ../lib { inherit inputs; inherit (inputs.nixpkgs) lib; };
  buildPlatforms = inputs.nixpkgs.lib.systems.flakeExposed;
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
      allConfigs = import ./allConfigs.nix {
        inherit pkgs lib nixosModules buildPlatform;
      };
    in
    {
      inherit nixosModules;
      legacyPackages.${buildPlatform} = pkgs;
      packages.${buildPlatform} = allConfigs // { default = pkgs.trilby-cli; };
      checks.${buildPlatform} = lib.optionalAttrs (lib.hasSuffix "-linux" buildPlatform)
        (
          let tests = import ../tests { inherit lib nixosModules buildPlatform pkgs; };
          in tests // { all = pkgs.linkFarm "trilby-checks" (tests // { inherit (pkgs) trilby-cli; }); }
        );
      formatter.${buildPlatform} = pkgs.writeShellApplication {
        name = "formatter";
        runtimeInputs = with pkgs; with haskellPackages; [
          cabal-gild
          fd
          fourmolu
          nixpkgs-fmt
        ];
        text = ''
          fd --extension=nix -X nixpkgs-fmt
          fd --extension=hs -X fourmolu -i
          fd --extension=cabal -x cabal-gild --io
        '';
      };
    }
  ))
]
