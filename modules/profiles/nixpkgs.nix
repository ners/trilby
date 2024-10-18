{ trilby, lib, ... }:

let
  pkgs = lib.pkgsFor trilby;
in
{
  nixpkgs = lib.mkMerge [
    {
      inherit (pkgs) overlays;
      inherit (trilby) hostPlatform;
    }
    (lib.optionalAttrs (trilby.buildPlatform != trilby.hostPlatform) {
      inherit (trilby) buildPlatform;
    })
    (lib.optionalAttrs (trilby ? variant && trilby.variant == "musl") {
      pkgs = pkgs.pkgsMusl;
    })
  ];
}
