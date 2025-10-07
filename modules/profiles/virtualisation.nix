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
      qemu.runAsRoot = false;
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
