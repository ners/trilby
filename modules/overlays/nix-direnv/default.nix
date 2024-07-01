{ lib, ... }:

final: prev: {
  nix-direnv = prev.nix-direnv.override {
    resholve = prev.resholve // {
      mkDerivation = attrs: prev.resholve.mkDerivation (attrs // {
        patches = (attrs.patches or [ ]) ++ [
          ./nvd.patch
        ];
        solutions = lib.recursiveUpdate attrs.solutions {
          default = {
            inputs = with final; [ findutils nvd ] ++ attrs.solutions.default.inputs;
            execer = [ "cannot:${final.nvd}/bin/nvd" ] ++ attrs.solutions.default.execer;
          };
        };
      });
    };
  };
}
