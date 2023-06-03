{ pkgs, ... }:

{
  home.pointerCursor = {
    name = "Vanilla-DMZ";
    package = pkgs.vanilla-dmz;
    size = 16;
    gtk.enable = true;
    x11.enable = true;
  };
}
