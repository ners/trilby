{ trilby, lib, pkgs, ... }:

lib.optionalAttrs (trilby.hostSystem.kernel.name == "linux") ({
  imports = with trilby.inputs.self.nixosModules; [
    profiles.base
    profiles.firefox
    profiles.fonts
    profiles.geoclue
    profiles.gnome
    profiles.libreoffice
    profiles.mimetypes
    profiles.pipewire
    profiles.sway
    profiles.virtualisation
  ];

  services = {
    flatpak.enable = true;
    fwupd.enable = true;
    printing.enable = true;
    usbmuxd.enable = true;
    xserver.enable = true;
  };

  systemd.oomd = {
    enable = lib.mkDefault true;
    enableUserSlices = lib.mkDefault true;
  };

  powerManagement.enable = true;

  security.rtkit.enable = true;
} //
lib.optionalAttrs (lib.versionAtLeast trilby.release "24.05") {
  # Both Gnome and Sway declare this as default, so let's resolve the ambiguity.
  programs.gnupg.agent.pinentryPackage = pkgs.pinentry-gnome3;
})
