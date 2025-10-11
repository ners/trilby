{
  inputs,
  lib,
  ...
}:

lib.makeExtensible (
  self:
  with builtins;
  with lib;
  lib
  // pipe ./. [
    filesystem.listFilesRecursive
    (filter (file: hasSuffix ".nix" file && file != ./default.nix))
    (map (
      file:
      import file {
        inherit inputs;
        lib = self;
      }
    ))
    (foldr recursiveUpdate { })
  ]
)
