{ inputs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.default
  ];

  nix.monitored.enable = true;
}
