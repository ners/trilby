{ trilby, inputs, ... }:

{
  imports = [
    inputs.nix-monitored.nixosModules.${trilby.hostPlatform}.default
  ];

  nix.monitored.enable = true;
}
