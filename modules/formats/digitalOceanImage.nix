{ modulesPath, lib, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/digital-ocean-image.nix"
  ];

  # not supported by systemd stage 1
  boot.growPartition = lib.mkForce false;
}
