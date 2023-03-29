{ trilby, inputs, config, ... }:

{
  imports = [
    inputs.home-manager.nixosModules.home-manager
  ];

  config.home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit inputs trilby;
      nixosConfig = config;
    };
  };
}
