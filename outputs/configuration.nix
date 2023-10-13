{ inputs, lib }:

trilby:

let
  pkgs = lib.pkgsFor {
    inherit (trilby) nixpkgs;
    system = trilby.hostPlatform;
  };
  system = lib.trilbySystem { inherit trilby; };
  name = trilby.configurationName;
in
lib.recursiveUpdate
{
  nixosConfigurations.${name} = system;
  packages.${trilby.buildPlatform}.${name} = system.config.system.build.${trilby.format};
}
  (lib.optionalAttrs (trilby.format == "isoImage") (
    lib.recursiveUpdate
    {
      checks.${trilby.buildPlatform}."${name}-test" = lib.trilbyTest {
        inherit name trilby;
        testScript = ''
          start_all()
          trilby.wait_for_unit('multi-user.target')
          trilby.succeed(" ".join([
            'trilby',
            '--verbosity debug',
            'install',
            '--disk /dev/vda',
            '--luks --luks-password foo',
            '--filesystem btrfs',
            '--edition server',
            '--channel unstable',
            '--host testhost',
            '--username testuser',
            '--password foo',
            '--no-reboot',
            '| logger'
          ]))
          trilby.succeed('findmnt /mnt')
          trilby.succeed('findmnt /mnt/home')
          trilby.succeed('findmnt /mnt/nix')
        '';
      };
    }
      (lib.foreach [ "gzip" "lzo" "lz4" "xz" "zstd" ]
        (algo:
        let
          system = lib.trilbySystem {
            inherit trilby;
            modules = [{ isoImage.squashfsCompression = algo; }];
          };
          name = "${trilby.configurationName}_${algo}";
          iso = system.config.system.build.${trilby.format};
        in
        {
          nixosConfigurations.${name} = system;
          packages.${trilby.buildPlatform} = {
            "${name}" = iso;
            "${name}-qemu" = (pkgs.writeShellApplication {
              name = "${name}-qemu";
              runtimeInputs = [ pkgs.qemu_full ];
              text = ''
                BIOS_FILE=''${BIOS_FILE:-${pkgs.OVMFFull.fd}/FV/OVMF.fd}
                ISO_FILE=''${ISO_FILE:-${iso}/iso/${iso.isoName}}
                DISK_FILE=''${DISK_FILE:-disk.qcow2}
                if ! [ -f "$DISK_FILE" ]; then
                    qemu-img create -f qcow2 "$DISK_FILE" 100G
                fi
                args=(
                    -name "''${NAME:-${name}}"
                    ''${GRAPHIC--nographic}
                    ''${BIOS--bios "$BIOS_FILE"}
                    ''${ISO--drive file="$ISO_FILE",format=raw,media=cdrom}
                    ''${DISK--drive file="$DISK_FILE",media=disk,if=virtio}
                    ''${SERIAL--serial mon:stdio}
                    -nodefaults
                    -enable-kvm
                    -machine q35,type=pc,accel=kvm,vmport=off,kernel_irqchip=on
                    -cpu host,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time,kvm=off,hv_vendor_id=1234567890ab
                    -smp cores=4
                    -m 4G
                    -device virtio-net,netdev=vmnic
                    -netdev user,id=vmnic
                    -device virtio-serial-pci
                    -boot order=cd
                )
                qemu-system-${trilby.hostSystem.cpu.name} "''${args[@]}"
              '';
            }).overrideAttrs (_: {
              preCheck = ''
                ignored=(
                  SC2054 #(warning): Use spaces, not commas, to separate array elements.
                  SC2086 #(info): Double quote to prevent globbing and word splitting.
                  SC2206 #(warning): Quote to prevent word splitting/globbing, or split robustly with mapfile or read -a.
                )
                printf -v opts '%s,' "''${ignored[@]}"
                export SHELLCHECK_OPTS="-e ''${opts%,}"
              '';
            });
          };
        })
      )
  ))
