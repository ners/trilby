{ trilby, ... }:

{
  imports = [
    (with trilby.nixpkgs.nixosModules.installer;
    if (trilby.hostSystem.cpu.arch == "riscv64") then
      sd-image.sd-image-riscv64-qemu
    else
      sd-image."sd-image-${trilby.system.cpu.arch}"
    )
  ];
}
