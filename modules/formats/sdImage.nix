{ trilby, modulesPath, ... }:

{
  imports = [
    (if (trilby.hostSystem.cpu.arch == "riscv64") then
      "${modulesPath}/installer/sd-card/sd-image-riscv64-qemu.nix"
    else
      "${modulesPath}/installer/sd-card/sd-image-x86_64.nix"
    )
  ];
}
