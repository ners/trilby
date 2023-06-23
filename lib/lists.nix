{ lib, ... }:

with builtins;
with lib;
{
  # Prepend a value to a list
  prepend = x: xs: [ x ] ++ xs;

  # Append a value to a list
  append = x: xs: xs ++ [ x ];

  # Return true if function `pred` returns false for all elements of `xs`.
  none = pred: xs: !(any pred xs);

  # A variant of foldl that has no base case, and thus may only be applied to non-empty lists.
  foldl1 = op: list: foldl op (head list) (tail list);

  # A variant of foldr that has no base case, and thus may only be applied to non-empty lists.
  foldr1 = op: list: foldr op (last list) (init list);
}
