{ trilby, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
  ];
  isoImage = {
    volumeID = with builtins; concatStringsSep "-" (filter (s: s != null && s != "") [
      trilby.name
      trilby.edition
      trilby.release
      trilby.hostSystem.cpu.name
      trilby.variant
    ]);
  };
  services.xserver.displayManager = {
    gdm.autoSuspend = false;
    autoLogin = {
      enable = true;
      user = "nixos";
    };
  };
}
