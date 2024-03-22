{ ... }:

final: prev: {
  nix-direnv = prev.nix-direnv.overrideAttrs (attrs: {
    prePatch = ''
      echo PATCHING
      pwd
      ls -alh
    '';
    patches = (attrs.patches or [ ]) ++ [
      ./nvd.patch
    ];
  });
}
