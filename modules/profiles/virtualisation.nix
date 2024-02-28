{ config, pkgs, lib, trilby, ... }:

{
  virtualisation = {
    podman = {
      enable = true;
      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;
      dockerSocket.enable = true;
      defaultNetwork =
        if lib.versionAtLeast trilby.release "23.05"
        then {
          settings.dns_enabled = true;
        } else {
          dnsname.enable = true;
        };
    };
    libvirtd = {
      enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      qemu = {
        ovmf = lib.mkMerge [
          {
            enable = true;
          }
          (lib.optionalAttrs (trilby.hostSystem.cpu.name == "x86_64") {
            packages = [
              (pkgs.OVMFFull.override {
                # https://github.com/NixOS/nixpkgs/pull/291963
                csmSupport = false; 
              }).fd
            ];
          })
        ];
        runAsRoot = false;
      };
    };
    spiceUSBRedirection.enable = true;
  };

  environment.systemPackages = with pkgs; [
    fuse-overlayfs
    libguestfs
    spice-vdagent
    swtpm
  ]
  ++ (lib.optional config.virtualisation.podman.enable podman-compose);

  boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
}
