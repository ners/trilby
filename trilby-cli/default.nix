{ pkgs
, lib
, ...
}:

let
  pname = "trilby-cli";
  haskellPackages = pkgs.callPackage ./haskell.nix { };
in
haskellPackages.${pname}
