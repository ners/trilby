{ inputs, ... }:

self: _: {
  inherit (inputs.disko.packages.${self.system}) disko;
}
