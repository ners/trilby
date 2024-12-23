{ trilby, lib, ... }@attrs:

squashfsCompression:

let
  name = "${trilby.configurationName}_${squashfsCompression}";
in
lib.recursiveConcat [
  (import ../system.nix {
    inherit lib name;
    inherit (trilby) buildPlatform format;
    system = lib.trilbySystem {
      inherit trilby;
      modules = [{ isoImage = { inherit squashfsCompression; }; }];
    };
  })
  (import ./qemu.nix (attrs // { inherit name; }))
]
