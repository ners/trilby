{ lib, ... }:

{
  globals.mapleader = lib.mkDefault " ";
  luaLoader.enable = true;
  clipboard.providers.wl-copy.enable = lib.mkDefault true;
  extraConfigLuaPre = builtins.readFile ../init.lua;
  plugins = {
    gitsigns.enable = lib.mkDefault true;
    lz-n.enable = lib.mkDefault true;
    nvim-autopairs.enable = lib.mkDefault true;
    scrollview.enable = lib.mkDefault true;
  };
}
