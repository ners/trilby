{ pkgs, lib, nixosModules, ... }@attrs:

with builtins;
let
  releases = (lib.loadFlake { src = ./releases; }).defaultNix.outputs.releases;
  allEditions = attrNames nixosModules.editions;
  allFormats = attrNames nixosModules.formats;
  allReleases = attrNames releases;
  configurations =
    { name ? [ "trilby" ]
    , edition ? allEditions
    , format ? allFormats
    , buildPlatform ? [ attrs.buildPlatform ]
    , hostPlatform ? [ attrs.buildPlatform ]
    , variant ? [ null ]
    , release ? allReleases
    }:
    lib.pipe { inherit name edition format buildPlatform hostPlatform variant release; } [
      lib.cartesianProduct
      (map (c: c // releases.${c.release}))
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
        ${system.trilby.configurationName} = system.config.system.build.${system.trilby.format} // {
          inherit (system.config.system.build) vm;
        };
      }
    );
in
packages // { trilby-all = pkgs.linkFarm "trilby-all" packages; } // lib.foreach allFormats (format: {
  "trilby-all-${format}" = lib.pipe format [
    (format: configurations { format = [ format ]; })
    (map (trilby: packages.${trilby.configurationName}))
    (pkgs.linkFarmFromDrvs "trilby-all-${format}")
  ];
})
