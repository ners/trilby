{ inputs, trilby, lib, ... }:

lib.optionalAttrs (trilby.edition == "server")
{
  imports = with inputs.self.nixosModules; [
    editions.base
    profiles.virtualisation
  ];

  boot.vesa = false;

  # Since we can't manually respond to a panic, just reboot.
  boot.kernelParams = [ "panic=1" "boot.panic_on_fail" ];

  # Don't allow emergency mode, because we don't have a console.
  systemd.enableEmergencyMode = false;

  # Being headless, we don't need a GRUB splash image.
  boot.loader.grub.splashImage = null;
}
