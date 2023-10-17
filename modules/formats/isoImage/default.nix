{ config, inputs, trilby, lib, pkgs, ... }:

{
  # Prefer our base config. This also prevents adding ZFS to `boot.supportedFilesystems` without forcing it.
  disabledModules = [ "profiles/base.nix" ];

  imports = [
    trilby.nixpkgs.nixosModules.installer.cd-dvd.installation-cd-base
    ./user.nix
  ];

  isoImage = lib.mkMerge [
    {
      volumeID = config.system.nixos.distroId or "${trilby.name}-${trilby.edition}";
      isoName = lib.mkForce "${config.isoImage.isoBaseName}-${config.system.nixos.label}-${pkgs.parsedSystem.cpu.name}.iso";
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
  environment.systemPackages = with pkgs; [
    cryptsetup
    disko
    efibootmgr
    efivar
    gptfdisk
    parted
    pciutils
    usbutils
  ];
  services.xserver.displayManager = {
    gdm.autoSuspend = false;
    autoLogin = {
      enable = true;
      user = "trilby";
    };
  };
  # Automatically log in at the virtual consoles.
  services.getty = {
    autologinUser = lib.mkForce "trilby";
    helpLine = lib.mkForce "";
  };
  services.openssh =
    if (lib.versionAtLeast trilby.release "23.05")
    then { settings.PasswordAuthentication = true; }
    else { passwordAuthentication = true; };

  users.motd = builtins.readFile ./motd.txt;
  boot.initrd = {
    luks.devices = { };
    systemd.enable = false;
  };
}
