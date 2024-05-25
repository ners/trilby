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
          inherit inputs lib trilby;
          overlays = overlaySrcs;
        })
        overlaySrcs;
    in
    import trilby.nixpkgs {
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
    } // t)
    (t: t // rec {
      name = toLower t.name;
      edition = toLower t.edition;
      hostSystem = { inherit (systems.parse.mkSystemFromString t.hostPlatform) kernel cpu; };
      nixpkgs = inputs.nixpkgs // {
        nixosModules = findModules "${inputs.nixpkgs}/nixos/modules";
      };
      release = nixpkgs.lib.trivial.release;
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
    if kernelName == "linux" then lib.nixosSystem systemAttrs
    else if kernelName == "darwin" then inputs.nix-darwin.lib.darwinSystem systemAttrs
    else throw "trilbySystem: unsupported kernel name: ${kernelName}";

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

  trilbyUser = u:
    let
      user = {
        uid = u.uid or 1000;
        name = u.name;
        isNormalUser = u.isNormalUser or true;
        home = u.home or "/home/${u.name}";
        createHome = u.createHome or true;
        group = u.group or u.name;
        extraGroups = u.extraGroups or [
          "audio"
          "networkmanager"
          "video"
          "wheel"
        ];
      }
      // (
        if (u ? initialPassword && u ? initialHashedPassword)
        then error "trilbyUser: both `initialPassword` and `initialHashedPassword` cannot be specified"
        else if (u ? initialPassword)
        then { inherit (u) initialPassword; }
        else if (u ? initialHashedPassword)
        then { inherit (u) initialHashedPassword; }
        else
          error "trilbyUser: required attribute `initialPassword` or `initialHashedPassword` missing"
      );
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
    {
      users.groups.${user.name} = group;
      users.users.${user.name} = user;
      home-manager.users.${user.name} = home;
    };
}
