{ lib
, unstable ? false
, ...
}:

final: prev:
if (lib.versionAtLeast prev.lib.trivial.release "25.05" || unstable) then
  (lib.loadFlake {
    src = ../../trilby-cli;
  }).defaultNix.outputs.overlays.default final
    prev
else
  {
    inherit (final.unstable) trilby-cli;
  }
