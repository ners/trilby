{ ... }:

final: prev: {
  nix-direnv = (prev.nix-direnv.override {
    resholve = final.resholve // {
      mkDerivation = attrs: final.resholve.mkDerivation (attrs // {
        solutions.default = attrs.solutions.default // {
          inputs = attrs.solutions.default.inputs ++ [
            final.findutils
            final.nvd
          ];
          execer = attrs.solutions.default.execer ++ [
            "cannot:${final.nvd}/bin/nvd"
          ];
        };
      });
    };
  }).overrideAttrs (attrs: {
    src = attrs.src.overrideAttrs (attrs: {
      patches = (attrs.patches or [ ]) ++ [
        ./nvd.patch
      ];
    });
  });
}
