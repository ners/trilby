{ inputs, system, lib, pkgs }@attrs:

let
  resolvedModules = lib.evalModules {
    modules = [
      # inputs.home-manager.nixosModules.default
      inputs.nixvim.homeManagerModules.nixvim
      {
        programs.nixvim = {
          luaLoader.enable = true;
          extraConfigLuaPre = builtins.readFile ../../home/neovim/init.lua;
        };
      }
    ];
    # ++ lib.findModulesList ../../home/neovim/plugins;
  };
in
inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
    inherit pkgs;
    module = with builtins; trace "PLUGINS ${toJSON (attrNames resolvedModules.config)}" {}; # resolvedModules.config.programs.nixvim;
}
