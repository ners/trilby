{ inputs, ... }:

{
  imports = [
    inputs.self.nixosModules.nixos.virtualisation.vmware-image
  ];
}
