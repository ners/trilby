{ modulesPath, lib, ... }:

{
  disabledModules = [
    lib.trilbyModules.profiles.bootloader
  ];

  imports = [
    "${modulesPath}/installer/netboot/netboot.nix"
    lib.trilbyModules.profiles.installer
  ];

  netboot.squashfsCompression = "lz4";
}
