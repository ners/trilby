{ lib, ... }:

with lib;
{
  # Pipes a value through a list of functions.
  # Produces a lambda that accepts a starting value when called with just a list of functions.
  pipef = flip pipe;

  # The unary function equivalent of ! operator
  not = x: !x;

  # Map each element of a list or attrset to an attrset, then flatten the attrsets.
  foreach = xs: f: foldr recursiveUpdate { } (
    if isList xs then map f xs
    else if isAttrs xs then mapAttrsToList f xs
    else error "foreach: expected list or attrset"
  );

  isEmpty = x: x == null || x == "" || x == [ ] || x == { };

  # A convenient attribute to debug which version of lib we are using.
  zzz = "zzz";
}
