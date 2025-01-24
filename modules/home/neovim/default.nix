{ inputs, lib, ... }:

with builtins;
with lib;
{
  imports = [inputs.nixvim.homeManagerModules.nixvim] ++ findModulesList ./plugins;

  programs.nixvim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    luaLoader.enable = true;
    extraConfigLuaPre = readFile ./init.lua;
  };
}
