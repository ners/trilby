{
  networking = {
    networkmanager.enable = true;
    useNetworkd = true;
    firewall.enable = true;
    # speed up boot
    dhcpcd = {
      wait = "background";
      extraConfig = "noarp";
    };
  };

  services.resolved = {
    enable = true;
    dnssec = "false";
  };

  systemd = {
    # use systemd networkd
    network = {
      enable = true;
      wait-online.anyInterface = true;
    };
    services = {
      # speed up boot
      NetworkManager-wait-online.enable = false;
      systemd-udev-settle.enable = false;
    };
  };
}
