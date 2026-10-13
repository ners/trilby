{ trilby, lib, ... }:

{
  networking = {
    networkmanager.enable = true;
    firewall.enable = true;
    nftables.enable = true;
  };

  services.resolved.enable = true;

  systemd.services = {
    NetworkManager-wait-online.enable = lib.mkDefault (trilby.edition == "server");
    systemd-udev-settle.enable = false;
  };
}
