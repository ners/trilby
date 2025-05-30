{ trilby, inputs, config, lib, ... }:

{
  imports =
    (lib.optional
      (trilby.hostSystem.kernel.name == "linux")
      trilby.home-manager.nixosModules.home-manager)
    ++ (lib.optional
      (trilby.hostSystem.kernel.name == "darwin")
      trilby.home-manager.darwinModules.home-manager);

  config.home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {
      inherit inputs trilby;
      lib = lib // inputs.home-manager.lib;
      nixosConfig = config;
    };
  };
}
