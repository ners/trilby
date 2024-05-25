{ inputs, ... }:

{
  imports = [
    inputs.home-manager.darwinModules.default
    inputs.self.nixosModules.profiles.nix
    inputs.self.nixosModules.profiles.nixpkgs
    inputs.self.nixosModules.profiles.users
    inputs.self.nixosModules.profiles.zsh
  ];
}
