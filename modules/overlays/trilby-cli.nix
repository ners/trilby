{ lib , ... }:

let
  flake = lib.loadFlake {
    src = ../../trilby-cli;
  };
in
final: prev:
{
  inherit (flake.defaultNix.outputs.legacyPackages.${prev.system}) trilby-cli;
}
