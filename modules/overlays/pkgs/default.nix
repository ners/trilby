{ lib, ... }:

final: prev: with lib; flip mapAttrs (findModules ./.) (_: module: prev.callPackage module { })
