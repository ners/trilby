{ trilby, config, lib, pkgs, modulesPath, ... }:

{
  # for virtio kernel drivers
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/trilby";
    autoResize = true;
    fsType = "btrfs";
  };

  boot = {
    growPartition = true;
    kernelParams = [ "console=ttyS0" ];
    loader = {
      grub.device =
        if (trilby.hostPlatform == "x86_64-linux") then
          (lib.mkDefault "/dev/vda")
        else
          (lib.mkDefault "nodev");

      grub.efiSupport = lib.mkIf (trilby.hostPlatform != "x86_64-linux") (lib.mkDefault true);
      grub.efiInstallAsRemovable = lib.mkIf (trilby.hostPlatform != "x86_64-linux") (lib.mkDefault true);
      timeout = 0;
    };
  };

  system.build.qcow = import "${modulesPath}/../lib/make-disk-image.nix" {
    inherit lib config pkgs;
    diskSize = 8192;
    format = "qcow2";
    partitionTableType = "hybrid";
  };
}
