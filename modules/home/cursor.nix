{ pkgs, ... }:

{
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.unstable.adwaita-icon-theme;
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };
}
