{ config, lib, ... }:

with builtins;
with lib;
let
  cfg = config.xdg.mimeApps.inverted;
  addMime =
    desktopNames: mime: attrs:
    attrs
    // {
      ${mime} = (attrs.${mime} or [ ]) ++ lib.splitString ";" desktopNames;
    };
  invert =
    with lib;
    foldlAttrs (
      acc: desktopNames: mimes:
      if isList mimes then foldr (addMime desktopNames) acc mimes else addMime desktopName mimes acc
    ) { };
in
{
  options.xdg.mimeApps.inverted = {
    defaultApplications = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
    };
    addedAssociations = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
    };
    removedAssociations = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
    };
  };

  config.xdg.mimeApps = {
    defaultApplications = invert cfg.defaultApplications;
    associations = {
      added = invert cfg.addedAssociations;
      removed = invert cfg.removedAssociations;
    };
  };
}
