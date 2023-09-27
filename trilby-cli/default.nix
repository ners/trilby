{ inputs, haskellPackages, ... }:

let
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
    nativeBuildInputs = with haskellPackages; [
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
