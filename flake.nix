{
  description = "Trilby: a NixOS-based operating system based on Fedora";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://trilby.cachix.org";
    extra-trusted-public-keys = "trilby.cachix.org-1:47uj9Bdgk9jCfhnY7ZDJlRSNJ/y5RkU6wBaEmGn9uns=";
  };

  inputs = {
    "nixpkgs-22.05".url = "github:nixos/nixpkgs/nixos-22.05";
    "nixpkgs-22.11".url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-monitored = {
      url = "github:ners/nix-monitored";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs:
    with builtins;
    let
      lib = import ./lib { inherit inputs; };
      buildPlatforms = attrNames inputs.nixpkgs-unstable.legacyPackages;
      nixosModules = {
        trilby = lib.findModules ./modules;
        nixos = lib.findModules "${inputs.nixpkgs-unstable}/nixos/modules";
      };
    in
    {
      inherit lib nixosModules;
    }
    // lib.foreachMapAttrs
        lib.trilbyConfig
        {
          name = [ "trilby" ];
          edition = attrNames nixosModules.trilby.editions;
          format = attrNames nixosModules.trilby.formats;
          hostPlatform = attrNames nixosModules.trilby.hostPlatforms;
          buildPlatform = buildPlatforms;
          variant = [ null "musl" ];
          channel = with lib; pipe inputs [
            attrNames
            (filter (hasPrefix "nixpkgs-"))
            (map (removePrefix "nixpkgs-"))
            (map (splitString "."))
            (map (concatStringsSep "_"))
          ];
        }
        (trilby: rec {
          nixosConfigurations.${trilby.configurationName} = lib.trilbySystem {
            inherit trilby;
          };
          packages.${trilby.buildPlatform}.${trilby.configurationName} =
            nixosConfigurations.${trilby.configurationName}.config.system.build.${trilby.format};
        })
    // lib.foreach buildPlatforms (buildPlatform:
      let
        pkgs = inputs.nixpkgs-unstable.legacyPackages.${buildPlatform};
      in
      {
        formatter.${buildPlatform} = pkgs.nixpkgs-fmt;
        devShells.${buildPlatform}.default = pkgs.mkShell {
          packages = with pkgs; [ nixpkgs-fmt ];
        };
      }
    );
}
