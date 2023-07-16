{ config, inputs, trilby, lib, pkgs, ... }:

{
  # Prefer our base config. This also prevents adding ZFS to `boot.supportedFilesystems` without forcing it.
  disabledModules = [ "profiles/base.nix" ];

  imports = [
    trilby.nixpkgs.nixosModules.installer.cd-dvd.installation-cd-base
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
  environment.etc.trilby.source = ../../..;
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

  users.motd = builtins.readFile ./motd.txt;
  boot.initrd = {
    luks.devices = { };
    systemd.enable = false;
  };
  users.groups.trilby.gid = 1000;
  users.users.trilby = {
    uid = 1000;
    isNormalUser = true;
    name = "trilby";
    home = "/home/trilby";
    createHome = true;
    group = "trilby";
    extraGroups = [ "wheel" "networkmanager" "video" ];
    initialPassword = "trilby";
  };
  home-manager.users.trilby = {
    programs.home-manager.enable = false;
    home = {
      username = "trilby";
      homeDirectory = "/home/trilby";
      stateVersion = trilby.release;
    };
    imports = [
      inputs.self.nixosModules.home
    ];
  };
}
