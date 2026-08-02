{ trilby, ... }:

{
  imports = with trilby.inputs; with self.nixosModules; [
    home-manager.darwinModules.default
    profiles.base
    profiles.nix
    profiles.nixpkgs
    profiles.users
    profiles.zsh
  ];

  system.stateVersion = 5;
}
