{ inputs, lib, ... }:

let
  src = inputs.nix-filter.lib {
    root = ./.;
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
final: prev: {
  haskell = prev.haskell // {
    packageOverrides = lib.composeExtensions prev.haskell.packageOverrides (hfinal: hprev:
      with prev.haskell.lib.compose;
      {
        hnix = dontCheck (hfinal.callCabal2nix "hnix" inputs.hnix { });
        hnix-store-core = doJailbreak (hfinal.callCabal2nix "hnix-store-core" "${inputs.hnix-store}/hnix-store-core" { });
        hnix-store-db = hfinal.callCabal2nix "hnix-store-db" "${inputs.hnix-store}/hnix-store-db" { };
        hnix-store-nar = hfinal.callCabal2nix "hnix-store-nar" "${inputs.hnix-store}/hnix-store-nar" { };
        hnix-store-readonly = hfinal.callCabal2nix "hnix-store-readonly" "${inputs.hnix-store}/hnix-store-readonly" { };
        hnix-store-remote = hfinal.callCabal2nix "hnix-store-remote" "${inputs.hnix-store}/hnix-store-remote" { };
        hnix-store-tests = hfinal.callCabal2nix "hnix-store-tests" "${inputs.hnix-store}/hnix-store-tests" { };
        some = hfinal.some_1_0_6;
        trilby-cli = hfinal.callCabal2nix "trilby-cli" src { };
      }
    );
  };
}
