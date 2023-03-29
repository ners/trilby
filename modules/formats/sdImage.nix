{ trilby, inputs, ... }:

{
  imports = [
    (with inputs.self.nixosModules.nixos.installer;
    if (trilby.system.cpu.arch == "riscv64") then
      sd-image.sd-image-riscv64-qemu
    else
      sd-image."sd-image-${trilby.system.cpu.arch}"
    )
  ];
}
