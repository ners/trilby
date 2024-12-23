{ trilby, lib, pkgs, ... }:

{
  imports = [
    (lib.trilbyUser trilby {
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

  services = {
    # Automatically log in at the virtual consoles.
    getty = {
      autologinUser = lib.mkForce "trilby";
      helpLine = lib.mkForce "";
    };
    xserver.displayManager.gdm.autoSuspend = false;
    displayManager.autoLogin = {
      enable = true;
      user = "trilby";
    };
  };

  users.motd = builtins.readFile ./motd.txt;

  networking.hostName = "trilby";
}
