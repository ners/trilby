{ pkgs, lib, nixosModules, ... }@attrs:

with builtins;
let
  configurations =
    { name ? [ "trilby" ]
    , edition ? attrNames nixosModules.editions
    , format ? attrNames nixosModules.formats
    , buildPlatform ? [ attrs.buildPlatform ]
    , hostPlatform ? [ attrs.buildPlatform ]
    , variant ? [ null ]
    , nixpkgs ? attrValues ((lib.loadFlake { src = ./releases; }).defaultNix.inputs)
    }:
    lib.pipe { inherit name edition format buildPlatform hostPlatform variant nixpkgs; } [
      lib.cartesianProduct
      (map lib.trilbyConfig)
      (filter (trilby: trilby.hostSystem.kernel.name != "darwin"))
    ];
  packages =
    lib.foreach (configurations { }) (trilby:
      let
        system = lib.trilbySystem {
          inherit trilby;
        };
      in
      {
        ${system.trilby.configurationName} = system.config.system.build.toplevel // {
          inherit (system.config.system.build) vm;
        };
      }
    );
  allToplevels =
    map
      (trilby: packages.${trilby.configurationName})
      (configurations { format = [ "toplevel" ]; });
  allIsoImages =
    map
      (trilby: packages.${trilby.configurationName})
      (configurations { format = [ "isoImage" ]; });
in
packages // {
  allToplevels = pkgs.linkFarmFromDrvs "allToplevels" allToplevels;
  allIsoImages = pkgs.linkFarmFromDrvs "allIsoImages" allIsoImages;
}
