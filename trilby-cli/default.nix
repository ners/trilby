attrs@{ inputs, pkgs, haskellPackages, ... }:

let
  haskellPackages = attrs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hnix = appendPatches super.hnix [
        # Fix pretty printer closing brace alignment
        (pkgs.fetchpatch {
          url = "https://github.com/haskell-nix/hnix/commit/5f3c357eee2943c6d0120fc40f3aff47509e5d60.patch";
          hash = "sha256-L+kUCgTK2P3i8oDRxcbNWcnbGO6SOVN6XgYM0AlR8fs=";
        })
        # Fix quasiquote free variable interpolation
        (pkgs.fetchpatch {
          url = "https://github.com/haskell-nix/hnix/commit/56df812a436df5e4f41ceb101dc62b3976c717a2.diff";
          hash = "sha256-ZrzRoH1riTiY1sT+0t+1RLGNNF3juPs82bI93wagRFw=";
        })
      ];
    };
  };
  pname = "trilby-cli";
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
  trilby-cli = haskellPackages.callCabal2nix pname src { };
  shell = haskellPackages.shellFor {
    packages = _: [ trilby-cli ];
    nativeBuildInputs = with pkgs; with haskellPackages; [
      cabal-install
      fourmolu
      cabal-fmt
      haskell-debug-adapter
      haskell-language-server
    ];
    withHoogle = true;
  };
in
trilby-cli // { inherit shell; }
