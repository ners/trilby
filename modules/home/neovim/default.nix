{ lib, ... }:

with builtins;
with lib;
{
  imports = attrValues (findModules ./plugins);

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

  xdg.configFile = pipe ./plugins [
    readDir
    attrNames
    (filter (hasSuffix ".lua"))
    (names: foreach names (name: {
      "nvim/plugin/${name}".source = ./plugins/${name};
    }))
  ];
}
