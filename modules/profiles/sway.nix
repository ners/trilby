{ pkgs, ... }:

{
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      foot
      rofi
      swayidle
      swaylock
    ];
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  services.blueman.enable = true;
  systemd.user.services.blueman-applet = {
    overrideStrategy = "asDropin";
    wantedBy = [ "sway-session.target" ];
    partOf = [ "sway-session.target" ];
  };
  environment.etc."xdg/autostart/blueman.desktop".text = ''
    [Desktop Entry]
    Type=Application
    Name=Blueman Applet
    Hidden=true
  '';
}
