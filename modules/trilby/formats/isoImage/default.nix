{ inputs, trilby, modulesPath, config, lib, ... }:

{
  imports = [
    (
      if trilby.edition == "workstation" then
        "${modulesPath}/installer/cd-dvd/installation-cd-graphical-base.nix"
      else
        "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
    )
    inputs.self.nixosModules.profiles.dvorak
  ];
  isoImage = {
    volumeID = with builtins; concatStringsSep "-" (filter (s: s != null && s != "") [
      trilby.name
      trilby.edition
      trilby.release
      trilby.hostSystem.cpu.name
      trilby.variant
    ]);
    squashfsCompression = "zstd";
  };
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

  users.motd = ''
    ${builtins.readFile ./motd.txt}

    OS:       Trilby ${lib.capitalise trilby.edition} ${trilby.release} (${config.system.nixos.codeName})
    Kernel:   ${config.boot.kernelPackages.kernel.system} ${config.boot.kernelPackages.kernel.version}
  '';
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
  home-manager.users.trilby = { ... }: {
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
