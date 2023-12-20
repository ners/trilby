{ config, lib, ... }:

let
  inherit (lib) mkDefault;
  inherit (config.boot.loader.efi) efiSysMountPoint;
  hasEfi = (config.fileSystems.${efiSysMountPoint}.fsType or "") == "vfat";
in
{
  boot = {
    loader = {
      # The number of seconds for user intervention before the default boot option is selected.
      timeout = mkDefault 3;
      efi.canTouchEfiVariables = hasEfi;
      grub = {
        enable = mkDefault (!hasEfi);
        efiSupport = mkDefault false;
        device = mkDefault "nodev";
        fsIdentifier = "label";
      };
      systemd-boot = {
        enable = mkDefault hasEfi;
        # The resolution of the console. A higher resolution displays more entries.
        consoleMode = "max";
      };
    };

    initrd = {
      # Use systemd for PID 1.
      systemd.enable = mkDefault true;
    };
  };
}
