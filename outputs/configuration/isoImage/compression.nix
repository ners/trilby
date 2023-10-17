{ trilby, lib, ... }@attrs:

squashfsCompression:

let
  name = "${trilby.configurationName}_${squashfsCompression}";
in
lib.recursiveConcat [
  (import ../system.nix {
    inherit (trilby) buildPlatform format;
    inherit name;
    system = lib.trilbySystem {
      inherit trilby;
      modules = [{ isoImage = { inherit squashfsCompression; }; }];
    };
  })
  (import ./qemu.nix (attrs // { inherit name; }))
]
