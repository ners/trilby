{ inputs, trilby, lib, ... }:

lib.optionalAttrs (trilby.edition == "workstation")
{
  imports = with inputs.self.nixosModules; [
    editions.base
    profiles.fonts
    profiles.geoclue
    profiles.gnome
    profiles.pipewire
    profiles.plymouth
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
    xserver.enable = true;
  };
  powerManagement.enable = true;
}
