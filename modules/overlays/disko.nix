{ inputs, trilby, ... }:

self: _: {
  inherit (inputs.disko.packages.${self.system or trilby.hostPlatform}) disko;
}
