{ trilby, ... }:

{
  imports = [
    trilby.nixpkgs.nixosModules.virtualisation.vmware-image
  ];
}
