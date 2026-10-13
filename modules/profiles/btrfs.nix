{ config, pkgs, lib, ... }:

lib.mkMerge [
  {
    boot = {
      supportedFilesystems = [ "btrfs" ];
      initrd.supportedFilesystems = [ "btrfs" ];
    };
    environment.systemPackages = with pkgs; [ btrfs-progs compsize ];
  }
  (lib.mkIf ((config.fileSystems."/".fsType or "") == "btrfs") {
    services.btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/" ];
    };
  })
]
