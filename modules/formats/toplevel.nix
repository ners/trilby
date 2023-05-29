{ lib, ... }:

{
  fileSystems = lib.mkDefault {
    "/" = {
      device = "/dev/sda";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };
  };
}
