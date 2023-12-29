{ inputs, lib, trilby, ... }:

self: super:
(import ../../trilby-cli/overlay.nix { inherit inputs lib; }) self super
  //
{
  trilby-cli = self.haskellPackages.trilby-cli.bin;
}
