{ config, pkgs, lib, ... }:

{
  boot = {
    supportedFilesystems = [ "btrfs" ];
    initrd.supportedFilesystems = [ "btrfs" ];
  };
  environment.systemPackages = with pkgs; [ btrfs-progs compsize ];
}
  //
lib.mkIf (config.fileSystems."/".fsType == "btrfs") {
  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/" ];
  };

  services.beesd.filesystems = {
    root = {
      spec = "LABEL=Trilby";
      hashTableSizeMB = 1024;
      verbosity = "crit";
      extraOptions = [ "--loadavg-target" "4.0" ];
    };
  };
}
