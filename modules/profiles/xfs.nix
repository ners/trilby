{ pkgs, ... }:

{
  boot.supportedFilesystems = [ "xfs" ];
  boot.initrd.supportedFilesystems = [ "xfs" ];
  environment.systemPackages = with pkgs; [ xfsprogs ];
}
