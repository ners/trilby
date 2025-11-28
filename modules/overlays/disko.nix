{ inputs, trilby ? null, ... }:

final: _: {
  inherit (inputs.disko.packages.${final.stdenv.hostPlatform.system or trilby.hostPlatform}) disko;
}
