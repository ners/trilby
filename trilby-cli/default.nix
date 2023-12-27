{ inputs, lib, pkgs, ... }:

let
  overlay = import ./overlay.nix { inherit inputs lib; };
  hp = (pkgs.extend overlay).haskellPackages;
  shell = hp.shellFor {
    packages = ps: [ ps.trilby-cli ];
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      fourmolu
      cabal-fmt
      haskell-debug-adapter
      haskell-language-server
    ];
  };
in
hp.trilby-cli // { inherit shell; }
