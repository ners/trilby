{ trilby, modulesPath, ... }:

{
  imports = [
    (
      if (trilby.system.cpu.arch == "riscv64") then
        "${modulesPath}/installer/sd-image/sd-image-riscv64-qemu.nix"
      else
        "${modulesPath}/installer/sd-image/sd-image-${trilby.system.cpu.arch}.nix"
    )
  ];
}
