{ pkgs, ... }:

{
  fileSystems."/".options = [
    "compress=zstd"
    "noatime"
    "nodiratime"
    "discard"
  ];

  fileSystems."/home".options = [
    "compress=zstd"
    "discard"
  ];

  fileSystems."/nix".options = [
    "compress=zstd"
    "noatime"
    "nodiratime"
    "discard"
  ];

  boot.initrd.supportedFilesystems = [ "btrfs" ];
  environment.systemPackages = with pkgs; [ btrfs-progs compsize ];

  services.beesd.filesystems = {
    root = {
      spec = "LABEL=Trilby";
      hashTableSizeMB = 256;
      verbosity = "crit";
      extraOptions = [ "--loadavg-target" "4.0" ];
    };
  };
}
