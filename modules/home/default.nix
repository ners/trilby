{ trilby, ... }:

{
  home.stateVersion = trilby.release;

  imports = [
    ./direnv.nix
    ./neovim
    ./starship.nix
    ./zsh
  ];
}
