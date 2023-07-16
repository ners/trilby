{ trilby, lib, ... }:

{
  home.stateVersion = trilby.release;

  imports = [
    ./cursor.nix
    ./dconf.nix
    ./direnv.nix
    ./firefox.nix
    ./git.nix
    ./neovim
    ./starship.nix
    ./xdg
    ./zsh
  ];

  # Home-manager's generation is currently broken
  # as it does not call modules with specialArgs.
  manual.manpages.enable = lib.mkForce false;
}
