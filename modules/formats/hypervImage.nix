{ inputs, ... }:

{
  imports = [
    inputs.self.nixosModules.nixos.virtualisation.hyperv-image
  ];
}
