{ trilby, ... }:

{
  home.stateVersion = trilby.release;

  imports = [
    ./direnv.nix
    ./firefox.nix
    ./neovim
    ./starship.nix
    ./zsh
  ];
}
