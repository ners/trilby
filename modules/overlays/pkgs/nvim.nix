{ inputs, lib, pkgs, ... }:

inputs.nixvim.legacyPackages.${pkgs.stdenv.hostPlatform.system}.makeNixvimWithModule {
  inherit pkgs;
  extraSpecialArgs = {
    inherit inputs;
    lib = lib.extend inputs.nixvim.lib.overlay;
  };
  module = {
    imports = lib.findModulesList ../../home/neovim/configuration;
  };
}
