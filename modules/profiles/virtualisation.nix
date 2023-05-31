{ pkgs, ... }:

{
  virtualisation = {
    podman.enable = true;
    libvirtd = {
      enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      qemu = {
        ovmf = {
          enable = true;
          packages = [ pkgs.OVMFFull.fd ];
        };
        runAsRoot = false;
      };
    };
    spiceUSBRedirection.enable = true;
  };

  environment.systemPackages = with pkgs; [
    libguestfs
    podman-compose
    spice-vdagent
    swtpm
  ];

  boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
}
