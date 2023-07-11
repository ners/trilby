{ inputs, lib, ... }:

{
  users.groups.$username.gid = 1000;
  users.users.$username = {
    uid = 1000;
    group = "$username";
    extraGroups = [
      "audio"
      "podman"
      "video"
      "wheel"
    ];
    home = "/home/$username";
    createHome = true;
    isNormalUser = true;
    initialPassword = "trilby";
  };
  home-manager.users.$username = {
    home.username = "$username";
    home.homeDirectory = "/home/$username";
    home.stateVersion = lib.mkForce "23.05";
    imports = [
      inputs.trilby.nixosModules.home
    ];
  };
}

