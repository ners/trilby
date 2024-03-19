{ inputs, ... }:

final: prev: {
  lib = import "${inputs.self}/lib" {
    inherit inputs;
    inherit (prev) lib;
  };
}
