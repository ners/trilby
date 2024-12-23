{ pkgs, lib, nixosModules, ... }@attrs:

with builtins;
let
  configurations =
    { name ? [ "trilby" ]
    , edition ? attrNames nixosModules.editions
    , format ? attrNames nixosModules.formats
    , buildPlatform ? [ attrs.buildPlatform ]
    , hostPlatform ? [ attrs.buildPlatform ]
    , variant ? [ null "musl" ]
    , nixpkgs ? attrValues ((lib.loadFlake { src = ./releases; }).defaultNix.inputs)
    }:
    lib.pipe { inherit name edition format buildPlatform hostPlatform variant nixpkgs; } [
      lib.cartesianProduct
      (map lib.trilbyConfig)
    ];
  packages =
    lib.foreach (configurations { }) (trilby:
      let
        system = lib.trilbySystem {
          inherit trilby;
        };
      in
      {
        ${system.trilby.configurationName} = system.config.system.build.${trilby.format};
      });
in
packages // {
  allToplevel = pkgs.linkFarmFromDrvs "allToplevel" (
    map
      (trilby: packages.${trilby.configurationName})
      (configurations {
        variant = [ null ];
        format = [ "toplevel" ];
      })
  );
  allIsoImage = pkgs.linkFarmFromDrvs "allIsoImage" (
    map
      (trilby: packages.${trilby.configurationName})
      (configurations {
        variant = [ null ];
        format = [ "isoImage" ];
      })
  );
}
