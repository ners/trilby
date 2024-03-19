{ lib
, trilby
, unstable ? false
, ...
}:

if (lib.versionAtLeast trilby.release "24.05" || unstable) then
  (lib.loadFlake {
    system = trilby.hostPlatform;
    src = ../../trilby-cli;
  }).defaultNix.outputs.overlays.default
else
  final: _: {
    inherit (final.unstable) trilby-cli;
  }
