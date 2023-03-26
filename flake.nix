{
  inputs = {
    "nixpkgs-22.05".url = "github:nixos/nixpkgs/nixos-22.05";
    "nixpkgs-22.11".url = "github:nixos/nixpkgs/nixos-22.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-monitored = {
      url = "github:ners/nix-monitored";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs:
    with builtins;
    let
      lib = inputs.nixpkgs-unstable.lib;
      buildPlatforms = attrNames inputs.nixpkgs-unstable.legacyPackages;
      configurations = {
        name = [ "trilby" ];
        edition = [ "server" "workstation" ];
        hostSystem = map lib.systems.parse.mkSystemFromString [
          "x86_64-linux"
          "aarch64-linux"
          "riscv64-linux"
        ];
        variant = [ null "musl" ];
        medium = [ null "iso" "sdimage" "vbox" "vmdk" ];
        channel = [ "unstable" "22.11" "22.05" ];
      };
      configurationName = c: concatStringsSep "-" (filter (s: s != null && s != "") [
        c.name
        c.edition
        c.channel
        c.hostSystem.cpu.name
        c.variant
        c.medium
      ]);
      foreach = xs: f: lib.foldr lib.recursiveUpdate { } (map f xs);
      foreachAttrs = attrs: foreach (lib.cartesianProductOfSets attrs);
    in
    foreach buildPlatforms (buildPlatform:
      let
        pkgs = inputs.nixpkgs-unstable.legacyPackages.${buildPlatform};
      in
      {
        formatter.${buildPlatform} = pkgs.nixpkgs-fmt;

        packages.${buildPlatform} = foreachAttrs configurations (c:
          let
            trilby = c // {
              configurationName = configurationName c;
            };
            nixos = inputs.self.nixosConfigurations.${trilby.configurationName};
          in
          {
            "${trilby.configurationName}" =
              if (trilby.medium == "iso") then
                nixos.config.system.build.isoImage
              else if (trilby.medium == "sdimage") then
                nixos.config.system.build.sdImage
              else
                nixos.config.system.build.toplevel;
          }
        );

        nixosConfigurations = foreachAttrs configurations (c:
          let
            trilby = c // {
              configurationName = configurationName c;
              inherit buildPlatform;
              hostPlatform = lib.systems.parse.doubleFromSystem c.hostSystem;
              inherit (nixpkgs.lib.trivial) release;
            };
            nixpkgs = inputs."nixpkgs-${trilby.channel}";
            hostPkgs = import nixpkgs {
              system = trilby.hostPlatform;
              overlays = [
                (self: super: {
                  parsedSystem = self.lib.systems.parse.mkSystemFromString self.system;
                  unstable = import inputs.nixpkgs-unstable {
                    system = trilby.hostPlatform;
                    config.allowUnfree = true;
                  };
                })
              ];
            };
          in
          {
            "${trilby.configurationName}" = nixpkgs.lib.nixosSystem {
              specialArgs = {
                inherit inputs trilby;
              };
              modules = [
                ./modules/trilby/medium.nix
                ./modules/trilby/${trilby.edition}.nix
                {
                  system.nixos.distroId = "${trilby.name}-${trilby.edition}";
                  nixpkgs = {
                    pkgs = hostPkgs;
                  } // lib.optionalAttrs (trilby.buildPlatform != trilby.hostPlatform) {
                    inherit (trilby) buildPlatform hostPlatform;
                  };
                }
              ];
            };
          }
        );
      }
    );
}
