{
  description = "Trilby: a NixOS-based operating system based on Fedora";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://cache.ners.ch/trilby";
    extra-trusted-public-keys = "trilby:AKUGezHi4YbPHCaCf2+XnwWibugjHOwGjH78WqRUnzU=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-unstable.follows = "nixpkgs";
    flake-compat.url = "github:ners/flake-compat";
    nix-monitored = {
      url = "github:ners/nix-monitored";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with builtins;
    let
      lib = import ./lib { inherit inputs; inherit (inputs.nixpkgs) lib; };
      buildPlatforms = attrNames inputs.nixpkgs.legacyPackages;
      nixosModules = lib.findModules ./modules;
      configurations = map lib.trilbyConfig (lib.cartesianProductOfSets {
        name = [ "trilby" ];
        edition = attrNames nixosModules.editions;
        format = attrNames nixosModules.formats;
        hostPlatform = attrNames nixosModules.hostPlatforms;
        buildPlatform = filter (lib.hasSuffix "-linux") buildPlatforms;
        variant = [ null "musl" ];
      });
    in
    lib.recursiveConcat [
      {
        inherit lib nixosModules;
      }
      (lib.foreach configurations (trilby:
        import ./outputs/configuration {
          inherit inputs trilby lib;
        }
      ))
      (lib.foreach buildPlatforms (buildPlatform:
        let
          pkgs = lib.pkgsFor {
            inherit buildPlatform;
            hostPlatform = buildPlatform;
          };
        in
        {
          formatter.${buildPlatform} = pkgs.nixpkgs-fmt;
          devShells.${buildPlatform} = {
            default = pkgs.mkShell {
              packages = with pkgs; [ nixpkgs-fmt ];
            };
          };
          legacyPackages.${buildPlatform} = pkgs;
          packages.${buildPlatform} = {
            default = pkgs.trilby-cli;
          } // import ./outputs/allConfigs.nix {
            inherit configurations buildPlatform inputs lib pkgs;
          };
        }
      ))
    ];
}
