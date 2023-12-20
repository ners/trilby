{ lib, pkgs, ... }:

{
  boot = {
    # Use the latest kernel!
    kernelPackages = lib.mkDefault pkgs.linuxPackages_latest;
  };
}
