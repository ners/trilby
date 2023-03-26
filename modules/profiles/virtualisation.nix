{ pkgs, ... }:

{
  virtualisation = {
    podman.enable = true;
    libvirtd = {
      enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      qemu = {
        ovmf.enable = true;
        runAsRoot = false;
      };
    };
    spiceUSBRedirection.enable = true;
  };

  environment.systemPackages = with pkgs; [
    libguestfs
    spice-vdagent
    swtpm
    podman-compose
  ];

  boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
}
