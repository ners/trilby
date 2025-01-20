{ inputs, modulesPath, ... }:

{
  disabledModules = [
    inputs.self.nixosModules.profiles.bootloader
  ];

  imports = [
    "${modulesPath}/installer/netboot/netboot.nix"
    inputs.self.nixosModules.profiles.installer
  ];

  netboot.squashfsCompression = "lz4";
}
