{ inputs, lib, ... }:

{
  imports = with inputs.self.nixosModules; [
    trilby.editions.base
  ];

  services.xserver.enable = lib.mkForce false;

  # Needed for NetworkManager
  services.dbus.enable = true;
}
