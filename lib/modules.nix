{ lib, ... }:

with builtins;
with lib;
{
  # Recurse into a directory tree to find modules and build a nested attrset, with names corresponding to directory
  # structure and values the full path to the module which can be imported.
  # A module is either a nix file not named default.nix, or a directory containing a default.nix.
  # Recursion stops when a module is found, e.g. a file lib.nix in a directory containing default.nix will not be found.
  findModules = dir: pipe dir [
    readDir
    attrsToList
    (foldr
      ({ name, value }: acc:
        let
          fullPath = dir + "/${name}";
          isNixModule = value == "regular" && hasSuffix ".nix" name && name != "default.nix";
          isDir = value == "directory";
          isDirModule = isDir && readDir fullPath ? "default.nix";
          module = nameValuePair (removeSuffix ".nix" name) (
            if isNixModule || isDirModule then fullPath
            else if isDir then findModules fullPath
            else { }
          );
        in
        if module.value == { } then acc
        else append module acc
      ) [ ])
    listToAttrs
    #(mapAttrs (name: value: trace "LOADING MODULE ${name}" value))
  ];
}
