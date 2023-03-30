{ inputs, ... }:

{
  imports = with inputs.self.nixosModules.trilby; [
    editions.base
    profiles.fonts
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
