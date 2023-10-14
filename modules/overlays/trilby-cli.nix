{ inputs, lib, trilby, unstable ? false, ... }:

self: super:
(
  {
    trilby-cli = self.callPackage ../../trilby-cli { inherit inputs; };
  }
    //
  (lib.optionalAttrs unstable (
    let pkgs = inputs.nixpkgs-unstable.legacyPackages.${trilby.hostPlatform}; in
    lib.foreach pkgs.haskell.packages (ghcName: haskellPackages: {
      "trilby-cli-${ghcName}" = pkgs.callPackage ../../trilby-cli {
        inherit inputs haskellPackages;
      };
    })
  ))
)
