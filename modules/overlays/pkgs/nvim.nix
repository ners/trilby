{ inputs, system, lib, pkgs }@attrs:

let
  resolvedModules =
    {
      imports = [
        # inputs.home-manager.nixosModules.default
        # inputs.nixvim.homeManagerModules.nixvim
      ]; # ++ lib.findModulesList ../../home/neovim/plugins;
      config = {
        luaLoader.enable = true;
        extraConfigLuaPre = builtins.readFile ../../home/neovim/init.lua;
      };
    };
  # ++ lib.findModulesList ../../home/neovim/plugins;
in
inputs.nixvim.legacyPackages.${system}.makeNixvimWithModule {
  inherit pkgs;
  module = resolvedModules; # with builtins; trace "PLUGINS ${toJSON (attrNames resolvedModules.config)}" { }; # resolvedModules.config.programs.nixvim;
}
