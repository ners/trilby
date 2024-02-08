{ lib, ... }:

with builtins;
with lib;
{
  imports = findModulesList ./plugins;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    extraConfig = mkBefore /*vim*/ ''
      syntax on
      command W w
    '';
    extraLuaConfig = mkBefore /*lua*/ ''
      -- bytecompile lua modules
      vim.loader.enable()

      -- load .exrc, .nvimrc and .nvim.lua local files
      vim.o.exrc = true
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
