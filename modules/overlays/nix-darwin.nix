{ trilby, inputs, ... }:

if (trilby.hostSystem.kernel.name == "darwin")
then inputs.nix-darwin.overlays.default
else _: _: { }
