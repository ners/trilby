{ ... }:

{
  networking = {
    networkmanager.enable = true;
    useNetworkd = true;
    firewall.enable = true;
    dhcpcd.wait = "background";
    dhcpcd.extraConfig = "noarp";
  };

  systemd.network.enable = true;
  systemd.services = {
    NetworkManager-wait-online.enable = false;
    systemd-udev-settle.enable = false;
  };
}
