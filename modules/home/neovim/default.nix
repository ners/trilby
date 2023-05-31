{ lib, ... }:

{
  imports = builtins.attrValues (lib.findModules ./plugins);

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    extraConfig = ''
      syntax on
      filetype on
      filetype plugin on
      filetype indent on
      command W w
    '';
  };
}
