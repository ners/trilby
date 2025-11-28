{ lib
, inputs
, overlays
, ...
}@args:

self: super: {
  unstable = import inputs.nixpkgs-unstable {
    inherit (super.stdenv.hostPlatform) system;
    config.allowUnfree = true;
    overlays = with builtins; lib.pipe overlays [
      (filter (o: o != ./unstable.nix))
      (map (o: import o (args // { unstable = true; })))
    ];
  };
}
