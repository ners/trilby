{ nixosConfig, lib, pkgs, ... }:

{
  programs.firefox = lib.mkIf (nixosConfig.services.xserver.enable) {
    enable = true;
    package = pkgs.unstable.firefox-devedition;
  };
}
