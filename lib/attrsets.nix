{ lib, ... }:

with builtins;
with lib;
{
  # Convert a single {name, value} pair to a corresponding attrset
  nameValuePairToAttrs = pipef [
    singleton
    listToAttrs
  ];

  # Modify the name field of an attrset by mapping it through a function.
  mapName =
    f: attrs:
    assert assertMsg (isFunction f) f;
    assert assertMsg (isAttrs attrs) attrs;
    attrs // { name = f attrs.name; };

  # Flatten an attrset by bringing all the leaves to the top level. As there is
  # no singular name flattening strategy, the result is a list of
  # NameValuePairs, where the name is a list of parts.
  #
  # The 'cond' predicate function specifies whether to recurse. Use
  # 'const true' to recurse all the way down.
  #
  # Example:
  # flattenAttrs (const true) { a = { b = 3; c = true; }; d = "paramecium"; }
  # => [ { name = ["a" "b"]; value = 3; } { name = ["a" "c"]; value = true; } { name = ["d"]; value = "paramecium"; } ]
  # flattenAttrs (x: not (hasAttr "b" x)) { a = { b = 3; c = true; }; d = "paramecium"; }
  # => [ { name = ["a"]; value = { b = 3; c = true; }; } { name = ["d"]; value = "paramecium"; } ]
  flattenAttrs =
    cond:
    pipef [
      (mapAttrsToList (name: value: nameValuePair (singleton name) value))
      (foldr (
        p@{ name, value }:
        acc:
        if isAttrs value && cond value then
          acc
          ++ pipe value [
            (flattenAttrs cond)
            (map (mapName (n: name ++ n)))
          ]
        else
          append p acc
      ) [ ])
    ];

  # Applies the merge function to the output of flattenAttrs, producing an attrset.
  # The merge function converts each {name, value} pair to one in which the name is
  # merged into a single string, and may change the value as well.
  flattenAttrsWith =
    cond: merge:
    pipef [
      (flattenAttrs cond)
      (map merge)
      (map nameValuePairToAttrs)
      recursiveConcat
    ];

  # Concats a list of attrsets into a single attrset, updating them recursively.
  recursiveConcat = foldr recursiveUpdate { };
}
