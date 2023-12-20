{ trilby, inputs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.default
  ];

  nix = {
    monitored.enable = true;
    settings = {
      auto-optimise-store = true;
      preallocate-contents = false;
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ "root" "@wheel" "@admin" ];
    };
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
      dates = "monthly";
    };
    registry = {
      nixpkgs.flake = trilby.nixpkgs;
      trilby.flake = inputs.self;
    };
    nixPath = [ "nixpkgs=/etc/channels/nixpkgs" ];
  };

  environment.etc."channels/nixpkgs".source = trilby.nixpkgs.outPath;
}
