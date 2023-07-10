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
