{ trilby, lib, pkgs, ... }:

{
  imports = [
    (lib.trilbyUser trilby {
      name = "trilby";
      initialPassword = "trilby";
    })
  ];

  users.users.nixos = {
    isNormalUser = lib.mkForce false;
    isSystemUser = true;
    group = "nogroup";
  } //
  lib.optionalAttrs (lib.versionAtLeast trilby.release "25.05") {
    enable = false;
  };

  environment.systemPackages = with pkgs; [
    acpi
    bench
    cryptsetup
    disko
    efibootmgr
    efivar
    gptfdisk
    hwinfo
    iperf
    ipmitool
    lm_sensors
    parted
    pciutils
    s-tui
    stress-ng
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
      if (lib.versionAtLeast trilby.release "25.11")
      then { displayManager.gdm.autoSuspend = false; }
      else { xserver.displayManager.gdm.autoSuspend = false; }
    )
  ];

  users.motd = builtins.readFile ./motd.txt;

  networking.hostName = "trilby";

  # Whitelist wheel users to do anything
  # This is useful for things like pkexec
  #
  # WARNING: this is dangerous for systems
  # outside the installation-cd and shouldn't
  # be used anywhere else.
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel")) {
        return polkit.Result.YES;
      }
    });
  '';
}
