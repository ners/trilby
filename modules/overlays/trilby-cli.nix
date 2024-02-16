{ inputs, lib, trilby, ... }:

final: prev:
let
  flake = lib.loadFlake {
    inherit (prev) system;
    src = ../../trilby-cli;
  };
  overlay = flake.defaultNix.outputs.overlays.default;
  pkgs = prev.extend overlay;
in
{
  trilby-cli = pkgs.haskellPackages.trilby-cli.bin;
}
