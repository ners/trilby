{
  trilby,
  lib,
  pkgs,
  ...
}:

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

  services = lib.mkMerge [
    {
      # Automatically log in at the virtual consoles.
      getty = {
        autologinUser = lib.mkForce "trilby";
        helpLine = lib.mkForce "";
      };
      displayManager.autoLogin = {
        enable = true;
        user = "trilby";
      };
    }
    (
      if (lib.versionAtLeast trilby.release "25.11") then
        { displayManager.gdm.autoSuspend = false; }
      else
        { xserver.displayManager.gdm.autoSuspend = false; }
    )
  ];

  users.motd = builtins.readFile ./motd.txt;

  networking.hostName = "trilby";
}
