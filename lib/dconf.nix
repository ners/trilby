{ lib, ... }:

with builtins;
with lib;
{
  dconfFlatten =
    let
      merge = { name, value }:
        assert assertMsg
          (length name > 1)
          "dconf configuration requires at least two-level names!";
        nameValuePair (concatStringsSep "/" (init name)) { ${last name} = value; };
    in
    flattenAttrsWith merge;
}
