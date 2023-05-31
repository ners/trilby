{ trilby, lib, ... }:

{
  home.stateVersion = trilby.release;

  imports = [
    ./dconf.nix
    ./direnv.nix
    ./firefox.nix
    ./neovim
    ./starship.nix
    ./xdg
    ./zsh
  ];

  # Home-manager's generation is currently broken
  # as it does not call modules with specialArgs.
  manual.manpages.enable = lib.mkForce false;
}
