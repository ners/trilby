{ inputs, trilby, ... }@attrs:

let
  lib = import ../../lib {
    inherit inputs;
    inherit (trilby.nixpkgs) lib;
  };
  pkgs = lib.pkgsFor trilby;
in
lib.recursiveConcat [
  (import ./system.nix {
    inherit lib;
    inherit (trilby) buildPlatform format;
    name = trilby.configurationName;
    system = lib.trilbySystem { inherit trilby; };
  })
  (lib.optionalAttrs (trilby.format == "isoImage") (
    import ./isoImage (attrs // { inherit lib pkgs; })
  ))
]
