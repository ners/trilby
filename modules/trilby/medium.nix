{ trilby, lib, modulesPath, ... }:

with builtins;
with lib;
let
  iso = optionals (trilby.medium == "iso") [
    (
      if (trilby.edition == "workstation") then
        "${modulesPath}/installer/cd-dvd/installation-cd-graphical-base.nix"
      else if (trilby.edition == "server") then
        "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
      else
        null
    )
    {
      isoImage = {
        volumeID = concatStringsSep "-" (filter (s: s != null && s != "") [
          trilby.name
          trilby.edition
          trilby.release
          trilby.hostSystem.cpu.name
          trilby.variant
        ]);
      };
    }
  ];
  sdimage = optionals (trilby.medium == "sdimage") [
    (
      if (trilby.system.cpu.arch == "riscv64") then
        "${modulesPath}/installer/sd-image/sd-image-riscv64-qemu.nix"
      else
        "${modulesPath}/installer/sd-image/sd-image-${trilby.system.cpu.arch}.nix"
    )
  ];
in
{
  imports = filter (s: s != null) (concatLists [ iso sdimage ]);
}
