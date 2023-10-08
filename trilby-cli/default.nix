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
          url = "https://github.com/haskell-nix/hnix/commit/16777e6a3ae08cd48179a3af51546671f19459f7.diff";
          hash = "sha256-1+HQWODW676a8Ft+xc0GLVrBThuzIhT+a3yU0ZJXVF8=";
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
