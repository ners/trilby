{ trilby, lib, ... }:

{
  imports = [ trilby.inputs.nixvim.homeModules.nixvim ];

  programs.nixvim = {
    enable = true;
    nixpkgs.source = trilby.inputs.nixpkgs;
    viAlias = true;
    vimAlias = true;
    imports = lib.findModulesList ./configuration;
  };
}
