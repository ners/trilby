{ inputs, ... }:

{
  imports = with inputs.self.nixosModules; [
    inputs.home-manager.darwinModules.default
    profiles.base
    profiles.nix
    profiles.nixpkgs
    profiles.users
    profiles.zsh
  ];

  system.stateVersion = 5;
}
