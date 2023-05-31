{ lib, ... }:

{
  xdg.configFile = with builtins; with lib; pipe ./. [
    filesystem.listFilesRecursive
    (filter (file: ! hasSuffix ".nix" file))
    (map (file: {
      name = removePrefix "${toString ./.}/" (toString file);
      value = { source = file; };
    }))
    listToAttrs
  ];
}
