{ trilby, config, lib, pkgs, modulesPath, ... }:

{
  disabledModules = [
    ../../btrfs.nix
  ];
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/trilby";
    autoResize = true;
    fsType = "f2fs";
  };

  boot = {
    growPartition = true;
    kernelParams = [ "console=ttyS0" ];
    loader = {
      grub.device = lib.mkDefault (
        if trilby.hostPlatform == "x86_64-linux" then
          "/dev/vda"
        else
          "nodev"
      );

      grub.efiSupport = lib.mkIf (trilby.hostPlatform != "x86_64-linux") (lib.mkDefault true);
      grub.efiInstallAsRemovable = lib.mkIf (trilby.hostPlatform != "x86_64-linux") (lib.mkDefault true);
      timeout = 0;
    };
  };

  system.build.qcow = import "${modulesPath}/../lib/make-disk-image.nix" {
    inherit lib config pkgs;
    diskSize = 20000;
    format = "qcow2-compressed";
    partitionTableType = "hybrid";
    label = "trilby";
  };
}
