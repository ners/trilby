{ trilby, ... }:

{
  imports = [
    trilby.nixosModules.virtualisation.virtualbox-image
  ];
}
