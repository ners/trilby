{ config, inputs, trilby, lib, pkgs, ... }:

let
  baseName = "${config.system.nixos.distroId}-${config.system.nixos.label}-${pkgs.parsedSystem.cpu.name}";
in
{
  imports = [
    trilby.nixpkgs.nixosModules.installer.cd-dvd.installation-cd-base
    inputs.self.nixosModules.profiles.installer
  ];

  isoImage = lib.mkMerge [
    {
      volumeID = config.system.nixos.distroId;
      grubTheme = pkgs.trilby-grub2-theme;
      splashImage = pkgs.runCommand "bios-boot.png"
        {
          buildInputs = with pkgs; [ imagemagick ];
        } ''
        convert ${inputs.self.nixosModules.overlays.trilby-grub2-theme}/bios-boot.svg $out
      '';
      storeContents = builtins.attrValues inputs;
    }
    (lib.optionalAttrs (lib.versionAtLeast trilby.release "23.05") {
      appendToMenuLabel = "";
      prependToMenuLabel = "Install ";
    })
  ];

  services.openssh =
    if (lib.versionAtLeast trilby.release "23.05")
    then { settings.PasswordAuthentication = true; }
    else { passwordAuthentication = true; };

  boot.initrd = {
    luks.devices = { };
    systemd.enable = false;
  };
} // lib.optionalAttrs (lib.versionAtLeast trilby.release "24.05") {
  system.installer.channel.enable = false;
} // lib.optionalAttrs (lib.versionAtLeast trilby.release "25.05") {
  image.baseName = lib.mkForce baseName;
}
