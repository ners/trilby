{ ... }:

{
  disko.devices = {
    disk = {
      "/dev/nvme0n1" = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              name = "ESP";
              start = "1M";
              end = "1GiB";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted";
                extraOpenArgs = [ "allow-discards" ];
                keyFile = "/tmp/luksPassword";
                content = {
                  type = "btrfs";
                  extraArgs = [ "-f" ];
                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = [ "compress=zstd" "autodefrag" "noatime" ];
                    };
                    "/home" = {
                      mountOptions = [ "compress=zstd" "autodefrag" ];
                    };
                    "/nix" = {
                      mountOptions = [ "compress=zstd" "autodefrag" "noatime" ];
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
