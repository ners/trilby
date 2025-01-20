{ inputs, trilby, config, lib, pkgs, modulesPath, ... }:

{
  disabledModules = [
    inputs.self.nixosModules.profiles.btrfs
  ];
  imports = [
    trilby.nixpkgs.nixosModules.profiles.qemu-guest
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/trilby";
    fsType = "f2fs";
  };

  boot = {
    #not supported by systemd-boot yet
    #growPartition = true;
    loader = {
      timeout = 0;
      grub =
        if (trilby.hostPlatform == "x86_64-linux") then {
          device = "/dev/vda";
        } else {
          device = "nodev";
          efiSupport = lib.mkDefault true;
          efiInstallAsRemovable = lib.mkDefault true;
        };
    };
  };

  system.build.qcow = lib.mkDefault (
    import "${modulesPath}/../lib/make-disk-image.nix" {
      inherit lib config pkgs;
      diskSize = 20000;
      format = "qcow2-compressed";
      partitionTableType = "hybrid";
      label = "trilby";
    }
  );
}
