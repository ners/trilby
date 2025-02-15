{ pkgs, lib, ... }@attrs:

with builtins;
let
  allEditions = attrNames lib.trilbyModules.editions;
  allFormats = attrNames lib.trilbyModules.formats;
  allNixpkgs = attrValues ((lib.loadFlake { src = ./releases; }).defaultNix.inputs);
  configurations =
    { name ? [ "trilby" ]
    , edition ? allEditions
    , format ? allFormats
    , buildPlatform ? [ attrs.buildPlatform ]
    , hostPlatform ? [ attrs.buildPlatform ]
    , variant ? [ null ]
    , nixpkgs ? allNixpkgs
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
        ${system.trilby.configurationName} = system.config.system.build.${system.trilby.format} // {
          inherit (system.config.system.build) vm;
        };
      }
    );
in
packages // { trilby-all = pkgs.linkFarm "trilby-all" packages; } // lib.foreach allFormats (format: {
  "trilby-all-${format}" = lib.pipe { format = [ format ]; } [
    configurations
    (map (trilby: packages.${trilby.configurationName}))
    (pkgs.linkFarmFromDrvs "trilby-all-${format}")
  ];
})
