{ lib, ... }:

{
  programs.git = {
    enable = lib.mkDefault true;
    difftastic.enable = lib.mkDefault true;
    extraConfig.log.date = lib.mkDefault "iso";
    signing.format = lib.mkDefault "openpgp";
  };
}
