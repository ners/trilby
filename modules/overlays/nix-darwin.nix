{ inputs, ... }:

final: prev:
if prev.stdenv.hostPlatform.isDarwin
then inputs.nix-darwin.overlays.default final prev
else { }
