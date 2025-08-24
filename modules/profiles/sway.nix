{ pkgs, ... }:

{
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      foot
      rofi-wayland
      swayidle
      swaylock
    ];
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };
}
