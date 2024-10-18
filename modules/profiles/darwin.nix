{ inputs, pkgs, ... }:

{
  imports = [
    inputs.home-manager.darwinModules.default
    inputs.self.nixosModules.profiles.nix
    inputs.self.nixosModules.profiles.nixpkgs
    inputs.self.nixosModules.profiles.users
    inputs.self.nixosModules.profiles.zsh
  ];

  environment.systemPackages = with pkgs; [
    expect
    file
    git
    jq
    nvd
    rsync
    tmux
    trilby-cli
    unzip
    wget
    zip
  ];
}
