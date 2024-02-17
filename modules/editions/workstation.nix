{ inputs, trilby, lib, ... }:

lib.optionalAttrs (trilby.edition == "workstation")
{
  imports = with inputs.self.nixosModules; [
    profiles.base
    profiles.firefox
    profiles.fonts
    profiles.geoclue
    profiles.gnome
    profiles.libreoffice
    profiles.mimetypes
    profiles.pipewire
    profiles.plymouth
    profiles.sway
    profiles.virtualisation
  ];

  services = {
    blueman.enable = true;
    flatpak.enable = true;
    fwupd.enable = true;
    localtimed.enable = true;
    printing.enable = true;
    redshift.enable = true;
    usbmuxd.enable = true;
    xserver = {
      enable = true;
      exportConfiguration = true;
    };
  };
  powerManagement.enable = true;
  security.rtkit.enable = true;
}
