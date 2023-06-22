{ trilby, ... }:

{
  imports = [
    trilby.nixpkgs.nixosModules.virtualisation.virtualbox-image
  ];
}
