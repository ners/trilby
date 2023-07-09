{ inputs, ... }:

self: super:
let
  inherit (super) lib;
  pname = "trilby-cli";
  src = inputs.nix-filter.lib {
    root = ../../trilby-cli;
    include = [
      "app"
      "assets"
      "src"
      "LICENCE"
      (inputs.nix-filter.lib.matchExt "cabal")
      (inputs.nix-filter.lib.matchExt "md")
    ];
  };
in
{
  haskell = super.haskell // {
    packageOverrides = lib.composeExtensions super.haskell.packageOverrides (hself: hsuper:
      with super.haskell.lib;
      let
        package = hself.callCabal2nix pname src { };
        shell = hself.shellFor {
          packages = _: [ package ];
          nativeBuildInputs = with hself; [
            cabal-install
            fourmolu
            cabal-fmt
            haskell-debug-adapter
            haskell-language-server
          ];
        };
      in
      {
        "${pname}" = package // { inherit shell; };
        shelly = dontCheck (hsuper.shelly_1_12_1);
      }
      //
      (lib.optionalAttrs (lib.versionAtLeast hsuper.ghc.version "9.6") {
        fourmolu = hsuper.fourmolu_0_13_0_0;
      })
    );
  };
  trilby-cli = self.haskell.packages.ghc96.trilby-cli;
  trilby-cli-static = self.pkgsStatic.haskell.packages.ghc96.native-bignum.trilby-cli;
}
