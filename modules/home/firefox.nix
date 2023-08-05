{ trilby, lib, pkgs, ... }:

{
  programs.firefox = lib.mkIf (trilby.edition == "workstation") {
    enable = true;
    package = pkgs.unstable.firefox-devedition;
  };
}
