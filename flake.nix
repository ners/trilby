{
  description = "Trilby: a NixOS-based operating system based on Fedora";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://cache.ners.ch/trilby";
    extra-trusted-public-keys = "trilby:AKUGezHi4YbPHCaCf2+XnwWibugjHOwGjH78WqRUnzU=";
  };

  inputs = {
    "nixpkgs-23.11".url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat.url = "github:ners/flake-compat";
    nix-monitored = {
      url = "github:ners/nix-monitored";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      inputs.nix-filter.follows = "nix-filter";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    hnix = {
      url = "github:haskell-nix/hnix";
      flake = false;
    };
    hnix-store = {
      url = "github:haskell-nix/hnix-store/core-0.6.1.0";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      lib = import ./lib { inherit inputs; };
      buildPlatforms = attrNames inputs.nixpkgs-unstable.legacyPackages;
      nixosModules = lib.findModules ./modules;
      configurations = map lib.trilbyConfig (lib.cartesianProductOfSets {
        name = [ "trilby" ];
        edition = attrNames nixosModules.editions;
        format = attrNames nixosModules.formats;
        hostPlatform = attrNames nixosModules.hostPlatforms;
        buildPlatform = filter (lib.hasSuffix "-linux") buildPlatforms;
        variant = [ null "musl" ];
        channel = with lib; pipe inputs [
          attrNames
          (filter (hasPrefix "nixpkgs-"))
          (map (removePrefix "nixpkgs-"))
        ];
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
          packages.${buildPlatform}.default = pkgs.trilby-cli;
        }
      ))
    ];
}
