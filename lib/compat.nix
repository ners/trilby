{ inputs, ... }:

{
  # { src, system } -> { defaultNix, defaultShell }
  loadFlake = inputs.flake-compat.lib;
}
