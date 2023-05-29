{ trilby, ... }:

{
  imports = [
    trilby.nixosModules.virtualisation.hyperv-image
  ];
}
