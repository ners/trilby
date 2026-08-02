{
  imports = [
    ./direnv.nix
    ./fzf.nix
    ./git.nix
    ./neovim
    ./starship.nix
    ./zsh
  ];

  disabledModules = [ "misc/fontconfig.nix" ];
}
