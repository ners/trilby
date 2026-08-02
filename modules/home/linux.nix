{ lib, ... }:

{
  imports = with lib; pipe ./. [
    findModulesList
    (remove ./linux.nix)
    (remove ./darwin.nix)
  ];
}
