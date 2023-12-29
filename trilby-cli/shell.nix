{ haskellPackages, ... }:

haskellPackages.shellFor {
  packages = ps: [ ps.trilby-cli ];
  nativeBuildInputs = with haskellPackages; [
    cabal-install
    fourmolu
    cabal-fmt
    haskell-debug-adapter
    haskell-language-server
  ];
}
