{ pkgs
, ...
}:

let
  pname = "trilby-cli";
  haskellPackages = pkgs.callPackage ./haskell.nix { };
in
haskellPackages.shellFor {
  packages = ps: [ ps."${pname}" ];
  nativeBuildInputs = with haskellPackages; [
    cabal-install
    fourmolu
    haskell-debug-adapter
    haskell-language-server
  ];
}
