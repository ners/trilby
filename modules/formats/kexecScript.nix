{ inputs, lib, modulesPath, ... }:

{
  disabledModules = [
    inputs.self.nixosModules.profiles.bootloader
  ];

  imports = [
    "${modulesPath}/installer/netboot/netboot.nix"
    inputs.self.nixosModules.profiles.installer
  ];

  netboot.squashfsCompression = "lz4";

  security.polkit.extraConfig = lib.mkBefore ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel")) {
        return polkit.Result.YES;
      }
    });
  '';

  services.openssh = {
    enable = lib.mkForce true;
    startAtBoot = lib.mkDefault true;
  };
}
