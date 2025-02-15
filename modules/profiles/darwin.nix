{ inputs, lib, ... }:

{
  imports = with lib.trilbyModules; [
    inputs.home-manager.darwinModules.default
    profiles.base
    profiles.nix
    profiles.nixpkgs
    profiles.users
    profiles.zsh
  ];

  system.stateVersion = 5;
}
