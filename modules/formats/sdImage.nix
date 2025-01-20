{ trilby, ... }:

{
  imports = [
    (with trilby.nixpkgs.nixosModules.installer;
    if (trilby.hostSystem.cpu.arch == "riscv64") then
      sd-card.sd-image-riscv64-qemu
    else
      sd-card."sd-image-${trilby.hostSystem.cpu.name}"
    )
  ];
}
