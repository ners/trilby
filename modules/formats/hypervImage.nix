{ trilby, ... }:

{
  imports = [
    trilby.nixpkgs.nixosModules.virtualisation.hyperv-image
  ];
}
