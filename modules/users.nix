{ trilby, inputs, config, lib, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit inputs trilby;
      lib = lib // inputs.home-manager.lib;
      nixosConfig = config;
    };
  };
}
