{ inputs, ... }:

final: prev:
if prev.stdenv.isDarwin
then inputs.nix-darwin.overlays.default final prev
else { }
