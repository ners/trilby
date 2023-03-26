{ lib, ... }:

with builtins;
with lib;
{
  # Utility function that converts an attrset to a list of {name, value} pairs.
  # Inverse of builtins.listToAttrs: listToAttrs (atrsToList attrs) == attrs
  attrsToList = mapAttrsToList nameValuePair;

  # Convert a single {name, value} pair to a corresponding attrset
  nameValuePairToAttrs = pipef [ singleton listToAttrs ];

  # Modify the name field of an attrset by mapping it through a function.
  mapName = f: attrs:
    assert assertMsg (isFunction f) f;
    assert assertMsg (isAttrs attrs) attrs;
    attrs // { name = f attrs.name; };

  # Flatten an attrset by bringing all the leaves to the top level.
  # As there is no singular name flattening strategy, the result is a list of
  # NameValuePairs, where the name is a list of parts.
  #
  # Example:
  # flattenAttrs { a = { b = 3; c = true; }; d = "paramecium"; }
  # => [ { name = ["a" "b"]; value = 3; } { name = ["a" "c"]; value = true; } { name = ["d"]; value = "paramecium"; } ]
  flattenAttrs = flattenAttrsCond (const true);

  # Like `flattenAttrs', but takes an additional predicate function that tells
  # it whether to recurse into an attribute set.
  flattenAttrsCond = cond: pipef [
    (mapAttrsToList (name: value: nameValuePair (singleton name) value))
    (foldr
      (p@{ name, value }: acc:
        if isAttrs value && cond value then
          acc ++ pipe value [
            flattenAttrs
            (map (mapName (n: name ++ n)))
          ]
        else append p acc
      )
      [ ])
  ];

  # Applies the merge function to the output of flattenAttrs, producing an attrset.
  # The merge function converts each {name, value} pair to one in which the name is
  # merged into a single string, and may change the value as well.
  flattenAttrsWith = merge: pipef [
    flattenAttrs
    (map (pipef [ merge nameValuePairToAttrs ]))
    (foldr recursiveUpdate { })
  ];
}
