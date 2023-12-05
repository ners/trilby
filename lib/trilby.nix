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
    (t: {
      name = "trilby";
      edition = "workstation";
      channel = "unstable";
      hostPlatform = "x86_64-linux";
      buildPlatform = "x86_64-linux";
      variant = null;
      format = null;
    } // t)
    (t: t // rec {
      name = toLower t.name;
      edition = toLower t.edition;
      channel = toLower t.channel;
      hostSystem = systems.parse.mkSystemFromString t.hostPlatform;
      nixpkgs = inputs."nixpkgs-${channel}" // {
        nixosModules = findModules "${inputs."nixpkgs-${t.channel}"}/nixos/modules";
      };
      release = nixpkgs.lib.trivial.release;
      configurationName = concatStringsSep "-" (filter (s: s != null && s != "") [
        name
        edition
        (concatStringsSep "_" (splitString "." t.channel))
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
    in
    lib.nixosSystem {
      modules = with inputs.self.nixosModules; [
        editions.${trilby.edition}
        hostPlatforms.${trilby.hostPlatform}
      ]
      ++ optional (trilby ? format && isNotEmpty trilby.format) formats.${trilby.format}
      ++ attrs.modules or [ ];
      specialArgs = { inherit inputs lib trilby; } // attrs.specialArgs or { };
    };

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

      hostPkgs = pkgsFor {
        inherit (trilby) nixpkgs;
        system = trilby.buildPlatform;
      };

      node = {
        specialArgs = { inherit inputs trilby lib; };
        pkgs = pkgsFor {
          inherit (trilby) nixpkgs;
          system = trilby.hostPlatform;
        };
        pkgsReadOnly = false;
      };

      nodes = (attrs.nodes or { }) // {
        trilby = {
          imports = with inputs.self.nixosModules; [
            editions.${trilby.edition}
            hostPlatforms.${trilby.hostPlatform}
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
      group = {
        gid = u.gid or user.uid;
      };
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
