{ config, trilby, inputs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.default
  ];

  nix = {
    channel.enable = false;
    monitored.enable = true;
    settings = {
      auto-optimise-store = true;
      preallocate-contents = false;
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ "root" "@wheel" "@admin" ];
      nix-path = config.nix.nixPath;
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
      dates = "monthly";
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
    nixPath = [
      "nixpkgs=${trilby.nixpkgs.outPath}"
      "trilby=${inputs.self.outPath}"
    ];
  };
}
