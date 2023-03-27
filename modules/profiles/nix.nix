{ trilby, inputs, lib, ... }:

{
  imports = lib.optionals (trilby.edition == "workstation") [
      inputs.self.nixosModules.profiles.nix-monitored
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
    registry.nixpkgs.flake = inputs.nixpkgs-unstable;
    nixPath = [ "nixpkgs=/etc/channels/nixpkgs" ];
  };

  environment.etc."channels/nixpkgs".source = inputs.nixpkgs-unstable.outPath;
}
