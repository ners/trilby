{
  description = "Trilby: a NixOS-based operating system based on Fedora";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://trilby.cachix.org";
    extra-trusted-public-keys = "trilby.cachix.org-1:47uj9Bdgk9jCfhnY7ZDJlRSNJ/y5RkU6wBaEmGn9uns=";
  };

  inputs = {
    "nixpkgs-22.11".url = "github:nixos/nixpkgs/nixos-22.11";
    "nixpkgs-23.05".url = "github:nixos/nixpkgs/nixos-23.05";
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
    lib.foldr1 lib.recursiveUpdate [
      {
        inherit lib nixosModules;
      }
      (lib.foreach configurations (trilby:
        let
          system = lib.trilbySystem { inherit trilby; };
          name = trilby.configurationName;
        in
        lib.recursiveUpdate
          {
            nixosConfigurations.${name} = system;
            packages.${trilby.buildPlatform}.${name} = system.config.system.build.${trilby.format};
          }
          (lib.optionalAttrs (trilby.format == "isoImage") (lib.foreach [ "gzip" "lzo" "lz4" "xz" "zstd" ] (algo:
            let
              system = lib.trilbySystem {
                inherit trilby;
                modules = [{ isoImage.squashfsCompression = algo; }];
              };
              name = "${trilby.configurationName}_${algo}";
            in
            {
              nixosConfigurations.${name} = system;
              packages.${trilby.buildPlatform}.${name} = system.config.system.build.${trilby.format};
            }
          )))
      ))
      (lib.foreach buildPlatforms (buildPlatform:
        let
          overlaySrcs = builtins.attrValues inputs.self.nixosModules.overlays;
          pkgs = import inputs.nixpkgs-unstable {
            system = buildPlatform;
            overlays = lib.pipe overlaySrcs [
              (map (o: import o {
                inherit lib inputs;
                overlays = overlaySrcs;
              }))
            ];
          };
        in
        {
          formatter.${buildPlatform} = pkgs.nixpkgs-fmt;
          devShells.${buildPlatform} = {
            default = pkgs.mkShell {
              packages = with pkgs; [ nixpkgs-fmt ];
            };
          };
          packages.${buildPlatform} = pkgs;
        }
      ))
    ];
}
