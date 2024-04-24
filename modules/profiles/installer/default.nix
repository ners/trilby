{ lib, pkgs, ... }:

{
  imports = [
    (lib.trilbyUser {
      name = "trilby";
      initialPassword = "trilby";
    })
  ];

  environment.systemPackages = with pkgs; [
    cryptsetup
    disko
    efibootmgr
    efivar
    gptfdisk
    parted
    pciutils
    usbutils
  ];

  services.displayManager.autoLogin = {
      enable = true;
      user = "trilby";
    };
  services.xserver.displayManager.gdm.autoSuspend = false;
  users.motd = builtins.readFile ./motd.txt;

  # Automatically log in at the virtual consoles.
  services.getty = {
    autologinUser = lib.mkForce "trilby";
    helpLine = lib.mkForce "";
  };

  networking.hostName = "trilby";
}
