{ lib, ... }:

with builtins;
with lib;
{
  dconfFlattenWith = f:
    let
      cond = x: not (hasAttr "_type" x);
      merge = { name, value }:
        assert assertMsg
          (length name > 1)
          "dconf configuration requires at least two-level names!";
        nameValuePair (concatStringsSep "/" (init name)) { ${last name} = f value; };
    in
    flattenAttrsWith cond merge;

  dconfFlatten = dconfFlattenWith id;
}
