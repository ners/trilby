{ trilby, lib, ... }:

let
  defaultTest = {
    inherit trilby;
    name = "trilby";
    extraDriverArgs = [ "--keep-vm-state" ];
    skipLint = true;
    modules = [
      {
        documentation.enable = false; # speed up eval
        systemd.services.mdmonitor.enable = false; # silence some weird warnings
        virtualisation = {
          useEFIBoot = true;
          cores = 8;
          memorySize = 2048;
          emptyDiskImages = [ 8192 ]; # create an 8 GiB disk /dev/vdb
        };
        fileSystems."/nix/store" = lib.mkForce {
          device = "nix-store";
          fsType = "9p";
          neededForBoot = true;
          options = [ "trans=virtio" "version=9p2000.L" "cache=loose" ];
        };
      }
    ];
  };

  mkTest = attrs: lib.trilbyTest (defaultTest // attrs);

  tests = with builtins; with lib; pipe ./. [
    readDir
    (filterAttrs (_: type: type == "directory"))
    (mapAttrs (name: _: ./${name}))
  ];

in
lib.foreach tests (testName: testDir:
let
  name = "${trilby.configurationName}-${testName}";
in
{
  checks.${trilby.buildPlatform}."${name}-test" = mkTest {
    testScript = builtins.readFile "${testDir}/script.py";
  };
}
)
