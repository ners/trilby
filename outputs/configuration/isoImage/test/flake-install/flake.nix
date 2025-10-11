{
  inputs.trilby = { };

  outputs =
    inputs:
    let
      inherit (inputs.trilby) lib;
    in
    {
      nixosConfigurations.test = lib.trilbySystem {
        trilby = {
          edition = "server";
          channel = "unstable";
          hostPlatform = builtins.currentSystem;
        };
        modules = [
          (
            { trilby, ... }:
            {
              imports = [
                trilby.nixpkgs.nixosModules.testing.test-instrumentation
              ];

              disko.devices.disk.vdb = {
                type = "disk";
                device = "/dev/vdb";
                content = {
                  type = "gpt";
                  partitions = {
                    boot = {
                      priority = 0;
                      size = "1M";
                      type = "EF02";
                    };
                    ESP = {
                      priority = 1;
                      size = "1G";
                      type = "EF00";
                      content = {
                        type = "filesystem";
                        format = "vfat";
                        mountpoint = "/boot";
                      };
                    };
                    Trilby = {
                      size = "100%";
                      content = {
                        type = "filesystem";
                        format = "xfs";
                        mountpoint = "/";
                      };
                    };
                  };
                };
              };
            }
          )
        ];
      };
    };
}
