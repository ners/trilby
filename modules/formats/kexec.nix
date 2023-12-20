{ inputs, modulesPath, config, pkgs, ... }:

let
  init = builtins.unsafeDiscardStringContext config.system.build.toplevel + "/init";
in
{
  disabledModules = [
    inputs.self.nixosModules.profiles.bootloader
  ];

  imports = [
    "${modulesPath}/installer/netboot/netboot.nix"
    inputs.self.nixosModules.profiles.installer
  ];

  system.build = rec {
    image = pkgs.runCommand "image" { buildInputs = [ pkgs.nukeReferences ]; } ''
      mkdir $out
      cp ${config.system.build.kernel}/${config.system.boot.loader.kernelFile} $out/kernel
      cp ${config.system.build.netbootRamdisk}/initrd $out/initrd
      echo "init=${init} ${toString config.boot.kernelParams}" > $out/cmdline
      nuke-refs $out/kernel
    '';
    kexec_script = pkgs.writeShellApplication {
      name = "kexec-nixos";
      runtimeInputs = with pkgs; [ cpio kexectools ];
      text = ''
        cd "$(mktemp -d)"
        pwd
        mkdir initrd
        pushd initrd
        if [ -e /ssh_pubkey ]; then
          cat /ssh_pubkey >> authorized_keys
        fi
        find . -type f | cpio -o -H newc | gzip -9 > ../extra.gz
        popd
        cat ${image}/initrd extra.gz > final.gz

        kexec -l ${image}/kernel --initrd=final.gz --append="init=${init} ${toString config.boot.kernelParams}"
        sync
        echo "executing kernel, filesystems will be improperly umounted"
        kexec -e
      '';
    };
    kexec = pkgs.callPackage "${pkgs.path}/nixos/lib/make-system-tarball.nix" {
      storeContents = [
        {
          object = config.system.build.kexec_script;
          symlink = "/kexec_nixos";
        }
      ];
      contents = [ ];
    };
  };

  boot = {
    initrd = {
      postMountCommands = ''
        mkdir -p /mnt-root/root/.ssh/
        cp /authorized_keys /mnt-root/root/.ssh/
      '';
      availableKernelModules = [ "ata_piix" "uhci_hcd" ];
    };
    kernel.sysctl."vm.overcommit_memory" = "1";
  };

  environment.variables.GC_INITIAL_HEAP_SIZE = "1M";
}
