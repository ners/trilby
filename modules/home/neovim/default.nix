{ inputs, lib, ... }:

{
  imports = [ inputs.nixvim.homeModules.nixvim ];

  programs.nixvim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    imports = lib.findModulesList ./configuration;
  };
}
