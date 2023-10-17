{ trilby, lib, ... }@attrs:

let
  pkgs = lib.pkgsFor {
    inherit (trilby) nixpkgs;
    system = trilby.hostPlatform;
  };
in
lib.recursiveConcat [
  (import ./system.nix {
    inherit (trilby) buildPlatform format;
    name = trilby.configurationName;
    system = lib.trilbySystem { inherit trilby; };
  })
  (lib.optionalAttrs (trilby.format == "isoImage") (
    import ./isoImage (attrs // { inherit pkgs; })
  ))
]
