{ trilby, inputs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.${trilby.hostPlatform}.default
  ];

  nixpkgs.overlays = [
    (self: super: rec {
      nix-monitored = inputs.nix-monitored.packages.${self.system}.default.override self;
      nix-direnv = super.nix-direnv.override {
        nix = nix-monitored;
      };
      nixos-rebuild = super.nixos-rebuild.override {
        nix = nix-monitored;
      };
    })
  ];

  nix = {
    settings = {
      auto-optimise-store = true;
      preallocate-contents = false;
      experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
      trusted-users = [ "root" "@wheel" "@admin" ];
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
      dates = "monthly";
    };
    monitored = {
      enable = true;
    };
    registry.nixpkgs.flake = inputs.nixpkgs-unstable;
    nixPath = [ "nixpkgs=/etc/channels/nixpkgs" ];
  };

  environment.etc."channels/nixpkgs".source = inputs.nixpkgs-unstable.outPath;
}
