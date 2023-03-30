{ pkgs, ... }:

{
  boot.initrd.supportedFilesystems = [ "btrfs" ];
  environment.systemPackages = with pkgs; [ btrfs-progs compsize ];

  services.beesd.filesystems = {
    root = {
      spec = "LABEL=Trilby";
      hashTableSizeMB = 1024;
      verbosity = "crit";
      extraOptions = [ "--loadavg-target" "4.0" ];
    };
  };
}
