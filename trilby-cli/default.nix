attrs@{ inputs, pkgs, haskellPackages, ... }:

let
  haskellPackages = attrs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; builtins.trace "GHC ${super.ghc.version}" {
      hnix = dontCheck (super.callCabal2nix "hnix" inputs.hnix { });
      hnix-store-core = super.hnix-store-core_0_6_1_0;
      hnix-store-remote = super.hnix-store-remote_0_6_0_0;
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
