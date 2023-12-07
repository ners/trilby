attrs@{ inputs, pkgs, haskellPackages, ... }:

let
  haskellPackages = attrs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; builtins.trace "GHC ${super.ghc.version}" {
      hnix = dontCheck (super.callCabal2nix "hnix" inputs.hnix { });
      hnix-store-core = doJailbreak (super.callCabal2nix "hnix-store-core" "${inputs.hnix-store}/hnix-store-core" { });
      hnix-store-db = super.callCabal2nix "hnix-store-db" "${inputs.hnix-store}/hnix-store-db" { };
      hnix-store-nar = super.callCabal2nix "hnix-store-nar" "${inputs.hnix-store}/hnix-store-nar" { };
      hnix-store-readonly = super.callCabal2nix "hnix-store-readonly" "${inputs.hnix-store}/hnix-store-readonly" { };
      hnix-store-remote = super.callCabal2nix "hnix-store-remote" "${inputs.hnix-store}/hnix-store-remote" { };
      hnix-store-tests = super.callCabal2nix "hnix-store-tests" "${inputs.hnix-store}/hnix-store-tests" { };
      some = super.some_1_0_6;
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
    nativeBuildInputs = with pkgs; with attrs.haskellPackages; [
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
