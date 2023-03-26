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
}
