{ lib, buildPlatform, nixosModules, pkgs }:

let
  releases = (lib.loadFlake { src = ../outputs/releases; }).defaultNix.outputs.releases;
  release = releases.unstable;

  trilby = {
    edition = "server";
    hostPlatform = buildPlatform;
    inherit buildPlatform;
    inputs = release;
  };

  trilbyConfig = lib.trilbyConfig trilby;

  testInstrumentation = "${release.nixpkgs}/nixos/modules/testing/test-instrumentation.nix";

  testOptions = pkgs.writeText "test-options.nix" /*nix*/ ''
    { lib, ... }:
    {
      imports = [ "${testInstrumentation}" ];
      documentation.enable = false;
      home-manager.users = lib.mkForce { };
      i18n.supportedLocales = lib.mkForce [ "en_GB.UTF-8/UTF-8" ];
      nix.monitored.enable = lib.mkForce false;
      services.btrfs.autoScrub.enable = lib.mkForce false;
    }
  '';

  referenceSystem = { filesystem, luks }: let rootDevice = if luks then "/dev/mapper/cryptroot" else "/dev/vda3"; in lib.trilbySystem {
    inherit trilby;
    modules = [
      ({ trilby, inputs, lib, ... }: {
        imports = [ "${testOptions}" (lib.trilbyUser trilby { name = "testuser"; initialPassword = "hunter2"; inherit inputs; }) ];
        fileSystems."/boot" = { device = "/dev/vda2"; fsType = "vfat"; };
      })
      (if filesystem == "btrfs" then {
        fileSystems."/" = { device = rootDevice; fsType = "btrfs"; options = [ "subvol=root" "compress=zstd" "noatime" ]; };
        fileSystems."/home" = { device = rootDevice; fsType = "btrfs"; options = [ "subvol=home" "compress=zstd" ]; };
        fileSystems."/nix" = { device = rootDevice; fsType = "btrfs"; options = [ "subvol=nix" "compress=zstd" "noatime" ]; };
      } else {
        fileSystems."/" = { device = "/dev/vda3"; fsType = filesystem; };
      })
      (lib.optionalAttrs luks {
        boot.initrd.luks.devices.cryptroot.device = "/dev/vda3";
      })
    ];
  };

  collectInputs = inputs:
    let node = i: { key = i.outPath; value = i; };
    in map (n: n.value) (builtins.genericClosure {
      startSet = map node inputs;
      operator = n: map node (lib.attrValues (n.value.inputs or { }));
    });

  inputOverrides = lib.concatStringsSep " " (map (name: "${name}=path:${release.${name}}") [
    "nixpkgs"
    "home-manager"
    "nixvim"
  ]);

  diskImage = "./target.qcow2";

  plainPkgs = import release.nixpkgs { system = buildPlatform; };

  installerNode = args: { pkgs, ... }: {
    imports = [
      nixosModules.profiles.installer
      "${release.nixpkgs}/nixos/tests/common/auto-format-root-device.nix"
    ];

    virtualisation = {
      inherit diskImage;
      diskSize = 12 * 1024;
      cores = 4;
      memorySize = 4096;
      emptyDiskImages = [ 8192 ];
      rootDevice = "/dev/vdb";
      fileSystems."/".autoFormat = true;
      writableStoreUseTmpfs = false;
    };

    nix.settings = {
      substituters = lib.mkForce [ "daemon" ];
      require-sigs = false;
      connect-timeout = 1;
      flake-registry = "";
    };

    system.extraDependencies =
      let allOutputs = p: if p ? outputs then map (o: p.${o}) p.outputs else [ p ];
      in
      collectInputs (
        lib.attrValues trilbyConfig.inputs
        ++ lib.concatMap lib.attrValues (lib.attrValues releases)
        ++ lib.attrValues (lib.loadFlake { src = ../trilby-cli; }).defaultNix.inputs
      )
      ++ [ (referenceSystem args).config.system.build.toplevel ]
      ++ (let
        c = (referenceSystem args).config;
        outputsOf = p: lib.concatMap (o: lib.optional (p ? ${o}) p.${o})
          ((p.meta.outputsToInstall or [ "out" ]) ++ c.environment.extraOutputsToInstall);
      in lib.concatMap outputsOf (c.environment.systemPackages))
      ++ allOutputs pkgs.nix-monitored
      ++ allOutputs pkgs.haskellPackages.cabal2nix-unstable
      ++ map (n: pkgs.haskellPackages.${n}.cabal2nixDeriver)
        [ "trilby-cli" "hnix" "path-io-effectful" "terminal-widgets" "text-rope-zipper" "terminal" ]
      ++ lib.concatMap allOutputs (with pkgs; [
        desktop-file-utils shared-mime-info texinfo libxml2 libxslt docbook5 docbook_xsl_ns
        kbd kmod libarchive brotli curl sudo lndir unionfs-fuse shellcheck-minimal libcap libcap-text-verifier
        nixos-artwork.wallpapers.simple-dark-gray-bottom
        (python3.withPackages (p: [ p.mistune ]))
        mypy zstd systemd
      ])
      ++ lib.concatMap allOutputs (with plainPkgs; [
        stdenv stdenvNoCC bintools stdenv.shellPackage dash
      ] ++ plainPkgs.stdenv.initialPath ++ [
        util-linux e2fsprogs mdadm zfs lvm2 bash jq gnused gawk coreutils-full
        parted gptfdisk dosfstools xfsprogs btrfs-progs cryptsetup
      ]);
  };

  targetNode = {
    virtualisation = {
      inherit diskImage;
      useBootLoader = true;
      useEFIBoot = true;
      useDefaultFilesystems = false;
      efi.keepVariables = false;
    };
    virtualisation.fileSystems."/" = {
      device = "/dev/disk/by-label/this-is-not-real-and-will-never-be-used";
      fsType = "ext4";
    };
  };

  mkInstallTest = name: { filesystem ? "ext4", luks ? false, boot ? true }:
    lib.trilbyTest {
      name = "trilby-installer-${name}";
      inherit trilby;
      skipLint = true;
      globalTimeout = 120;
      nodes = {
        installer = installerNode { inherit filesystem luks; };
        target = targetNode;
      };
      testScript = /*python*/ ''
        installer.start()
        installer.wait_for_unit("multi-user.target")
        installer.succeed("udevadm settle")

        with subtest("Install"):
            installer.copy_from_host("${testOptions}", "/tmp/test-options.nix")
            installer.succeed(
                "cat > /tmp/editor <<'EDITOR'\n"
                + "#!/bin/sh\n"
                + "cp /tmp/test-options.nix \"$(dirname \"$2\")/test-options.nix\"\n"
                + "EDITOR\n"
                + "chmod +x /tmp/editor"
            )
            status, _ = installer.execute(
                "EDITOR=/tmp/editor TRILBY_INPUT_OVERRIDES='${inputOverrides}' "
                + "trilby --debug install"
                + " --disk /dev/vda --format"
                + " ${if luks then "--luks --luks-password hunter2" else "--no-luks"}"
                + " --filesystem ${filesystem}"
                + " --edition server --release unstable"
                + " --hostname testhost"
                + " --keyboard us --locale en_GB.UTF-8 --timezone Europe/Zurich"
                + " --username testuser --password hunter2"
                + " --edit --no-reboot"
                + " > /tmp/install.log 2>&1"
            )
            if status != 0:
                print(installer.execute("tail -n 60 /tmp/install.log")[1])
                raise Exception("trilby install failed")

        ${if boot then /*python*/ ''
          installer.succeed("umount -R /mnt")
          installer.shutdown()

          target.state_dir = installer.state_dir
          target.start()
          ${lib.optionalString luks ''
          target.wait_for_console_text("Passphrase|Please enter")
          target.send_console("hunter2\n")
          ''}
          target.wait_for_unit("multi-user.target")

          with subtest("The installed system is what we asked for"):
              assert target.succeed("hostname").strip() == "testhost"
              target.succeed("test -f /etc/trilby/flake.nix")
              assert "en_GB.UTF-8" in target.succeed("locale; cat /etc/locale.conf")
              assert target.succeed("readlink /etc/localtime || timedatectl show -p Timezone --value").strip().endswith("Europe/Zurich")
              assert target.succeed("id -u testuser").strip() == "1000"
              assert target.succeed("findmnt -no FSTYPE /").strip() == "${filesystem}"
        '' else /*python*/ ''
          with subtest("The installed disk is what we asked for"):
              installer.succeed("test -f /mnt/etc/trilby/flake.nix")
              installer.succeed("test -f /mnt/boot/loader/loader.conf")
              assert installer.succeed("findmnt -no FSTYPE /mnt").strip() == "${filesystem}"
              ${lib.optionalString luks /*python*/ ''
                installer.succeed("cryptsetup isLuks /dev/vda3")
              ''}
              ${lib.optionalString (filesystem == "btrfs") /*python*/ ''
              subvolumes = installer.succeed("btrfs subvolume list /mnt")
              names = [l.split(" path ")[-1] for l in subvolumes.splitlines()]
              for name in ["root", "home", "nix"]:
                  assert name in names, subvolumes
              ''}
        ''}
      '';
    };
in
{
  "installer-ext4" = mkInstallTest "ext4" { };
  "installer-btrfs-luks" = mkInstallTest "btrfs-luks" { filesystem = "btrfs"; luks = true; boot = false; };
}
