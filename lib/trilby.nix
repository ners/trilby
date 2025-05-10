{ inputs, lib, ... }:

with builtins;
with lib;
rec {
  pkgsFor = t:
    let
      trilby = trilbyConfig t;
      overlaySrcs = attrValues inputs.self.nixosModules.overlays;
      overlays = map
        (o: import o {
          inherit inputs lib;
          overlays = overlaySrcs;
        })
        overlaySrcs;
    in
    traceVerbose "pkgsFor ${toJSON t}" import trilby.nixpkgs {
      inherit overlays;
      system = trilby.hostPlatform;
    };

  trilbyConfig = pipef [
    (t: traceVerbose "trilbyConfig: ${toJSON t}" t)
    (t: {
      name = "trilby";
      edition = "workstation";
      hostPlatform = t.hostPlatform;
      buildPlatform = t.buildPlatform or builtins.currentSystem or t.hostPlatform;
      variant = null;
      format = null;
      nixpkgs = inputs.nixpkgs;
      home-manager = inputs.home-manager;
    } // t)
    (t: t // rec {
      name = toLower t.name;
      edition = toLower t.edition;
      hostSystem = { inherit (systems.parse.mkSystemFromString t.hostPlatform) kernel cpu; };
      nixpkgs = t.nixpkgs // {
        nixosModules = findModules "${t.nixpkgs}/nixos/modules";
      };
      release = t.nixpkgs.lib.trivial.release;
      configurationName = concatStringsSep "-" (filter (s: s != null && s != "") [
        name
        edition
        (concatStringsSep "_" (splitString "." release))
        hostSystem.cpu.name
        t.variant
        t.format
      ]);
    })
  ];

  trilbySystem = attrs:
    let
      trilby = trilbyConfig (attrs.trilby or { });
      lib = import ../lib {
        inherit inputs;
        inherit (trilby.nixpkgs) lib;
      };
      kernelName = trilby.hostSystem.kernel.name;
      systemAttrs = traceVerbose "trilbySystem: ${toJSON trilby}" {
        modules = with inputs.self.nixosModules; [
          hostPlatforms.${trilby.hostPlatform}
        ]
        ++ optional (trilby ? format && isNotEmpty trilby.format) formats.${trilby.format}
        ++ attrs.modules or [ ];
        specialArgs = { inherit inputs lib trilby; } // attrs.specialArgs or { };
      };
    in
    (
      if kernelName == "darwin" then inputs.nix-darwin.lib.darwinSystem systemAttrs
      else lib.nixosSystem systemAttrs
    ) // { inherit trilby; };

  trilbyTest = attrs:
    let
      trilby = trilbyConfig (attrs.trilby or { });
      lib = import ../lib {
        inherit inputs;
        inherit (trilby.nixpkgs) lib;
      };
      nixosLib = import "${trilby.nixpkgs}/nixos/lib" {
        inherit lib;
      };
    in
    nixosLib.runTest {
      inherit (attrs) name testScript;
      extraDriverArgs = attrs.extraDriverArgs or [ ];
      skipLint = attrs.skipLint or false;

      hostPkgs = pkgsFor trilby;

      node = {
        specialArgs = { inherit inputs trilby lib; };
        pkgs = pkgsFor trilby;
        pkgsReadOnly = false;
      };

      nodes = (attrs.nodes or { }) // {
        trilby = {
          imports = with inputs.self.nixosModules; [
            editions.${trilby.edition}
            hostPlatforms.${trilby.hostPlatform}
            trilby.nixpkgs.nixosModules.testing.test-instrumentation
          ]
          ++ optional (trilby ? format && isNotEmpty trilby.format) formats.${trilby.format}
          ++ attrs.modules or [ ];
        };
      };
    };

  trilbyUser = trilby: u:
    let
      isDarwin = trilby.hostSystem.kernel.name == "darwin";
      isNixos = not isDarwin;
      user = lib.recursiveConcat [
        {
          uid = u.uid or 1000;
          name = u.name;
          home = u.home or (if isDarwin then "/Users/${u.name}" else "/home/${u.name}");
        }
        (lib.optionalAttrs isNixos (
          if (u ? initialPassword && u ? initialHashedPassword)
          then error "trilbyUser: both `initialPassword` and `initialHashedPassword` cannot be specified"
          else if (u ? initialPassword)
          then { inherit (u) initialPassword; }
          else if (u ? initialHashedPassword)
          then { inherit (u) initialHashedPassword; }
          else
            throw "trilbyUser: required attribute `initialPassword` or `initialHashedPassword` missing"
        ))
        (lib.optionalAttrs isNixos {
          isNormalUser = u.isNormalUser or true;
          createHome = u.createHome or true;
          group = u.group or u.name;
          extraGroups = u.extraGroups or [
            "audio"
            "networkmanager"
            "video"
            "wheel"
          ];
        })
      ];
      group.gid = u.gid or user.uid;
      home = {
        home = {
          username = user.name;
          homeDirectory = user.home;
        };
        imports = [
          inputs.self.nixosModules.home
        ] ++ (u.imports or [ ]);
      };
    in
    lib.recursiveConcat [
      {
        users.users.${user.name} = user;
        home-manager.users.${user.name} = home;
        home-manager.extraSpecialArgs = u.extraSpecialArgs or {};
      }
      (lib.optionalAttrs isNixos {
        users.groups.${user.name} = group;
      })
    ];
}
