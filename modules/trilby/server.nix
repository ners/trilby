{ lib, ... }:

{
  imports = [
    ./base.nix
  ];

  services.xserver.enable = lib.mkForce false;

  # Needed for NetworkManager
  services.dbus.enable = true;
}
