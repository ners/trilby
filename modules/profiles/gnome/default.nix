{ lib, pkgs, ... }:

{
  imports = lib.findModulesList ./.;

  services = {
    xserver = {
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
    };
    gnome = {
      core-os-services.enable = true;
      core-shell.enable = true;
      evolution-data-server.enable = true;
      gnome-keyring.enable = true;
      gnome-online-accounts.enable = true;
      sushi.enable = true;
    };
    dbus.enable = true;
    gvfs.enable = true;
    udev.packages = with pkgs.gnome; with pkgs; [
      gnome-settings-daemon
    ];
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  security.pam.services = {
    login.enableGnomeKeyring = true;
    gdm.enableGnomeKeyring = true;
  };

  environment.systemPackages = with pkgs.gnome; with pkgs; with gnomeExtensions; [
    adwaita-icon-theme
    appindicator
    gnome-connections
    gnome-tweaks
    gnome-usage
    nautilus
  ];

  xdg.mime.inverted.defaultApplications = {
    "org.gnome.Nautilus.desktop" = [ "inode/directory" ];
    "org.gnome.gedit.desktop" = [ "text/plain" ];
  };
}
