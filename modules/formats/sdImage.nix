{ trilby, ... }:

let
  inherit (trilby.hostSystem) cpu;
in
{
  imports = [
    (with trilby.nixpkgs.nixosModules.installer;
    if (cpu.arch == "riscv64") then
      sd-card.sd-image-riscv64-qemu
    else if (cpu.name == "armv7l") then
      sd-card.sd-image-armv7l-multiplatform
    else
      sd-card."sd-image-${cpu.name}"
    )
  ];
}
