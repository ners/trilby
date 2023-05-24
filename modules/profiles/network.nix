{ ... }:

{
  networking = {
    networkmanager.enable = true;
    useNetworkd = true;
    # NetworkManager replaces wpa_supplicant
    wireless.enable = false;
    firewall.enable = true;
    dhcpcd.wait = "background";
    dhcpcd.extraConfig = "noarp";
  };

  services.resolved.enable = true;
  services.resolved.dnssec = "false";

  systemd.network = {
    enable = true;
    wait-online.timeout = 0;
  };
  systemd.services = {
    systemd-udev-settle.enable = false;
  };
}
