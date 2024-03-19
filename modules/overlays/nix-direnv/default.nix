{ ... }:

final: prev: {
  nix-direnv = prev.nix-direnv.overrideAttrs (attrs: {
    patchFlags = [ "--verbose" ];
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
