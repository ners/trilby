{ pkgs
, lib
, ...
}:

let
  pname = "trilby-cli";
in
pkgs.haskell.packages.ghc96.override {
  overrides = self: super: with pkgs.haskell.lib; {
    "${pname}" = self.callCabal2nix pname ./. { };
  } // lib.optionalAttrs (lib.versionAtLeast super.ghc.version "9.6") {
    fourmolu = super.fourmolu_0_12_0_0;
    shelly = dontCheck (super.shelly_1_12_1);
  };
}
