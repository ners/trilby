{ config, trilby, inputs, lib, pkgs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.default
  ];

  nix = lib.mkMerge [
    {
      monitored = {
        enable = lib.mkDefault true;
        notify = lib.mkDefault pkgs.stdenv.hostPlatform.isLinux;
        package = lib.mkDefault (pkgs.unstable.nix-monitored.override {
          nix = pkgs.unstable.nixVersions.latest;
        });
      };
      optimise.automatic = true;
      settings = {
        experimental-features = [ "nix-command" "flakes" ];
        trusted-users = [ "root" "@wheel" "@admin" ];
      };
      gc = {
        automatic = true;
        options = "--delete-older-than 30d";
      };
      registry = {
        nixpkgs.to = {
          type = "path";
          path = trilby.nixpkgs.outPath;
        };
        trilby.to = {
          type = "path";
          path = inputs.self.outPath;
        };
      };
    }

    {
      nixPath = [
        "nixpkgs=${trilby.nixpkgs.outPath}"
        "trilby=${inputs.self.outPath}"
      ];
      settings.nix-path = config.nix.nixPath;
    }

    (lib.optionalAttrs (trilby.hostSystem.kernel.name == "linux") {
      channel.enable = false;
      gc.dates = "monthly";
    })

    (lib.optionalAttrs (trilby.hostSystem.kernel.name == "darwin") {
      gc.interval.Day = 1;
    })
  ];
}
