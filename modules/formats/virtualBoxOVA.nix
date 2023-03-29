{ inputs, ... }:

{
  imports = [
    inputs.self.nixosModules.nixos.virtualisation.virtualbox-image
  ];
}
