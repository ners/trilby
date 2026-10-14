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

  referenceSystem = { filesystem, luks, efi }: let rootDevice = if luks then "/dev/mapper/cryptroot" else "/dev/vda3"; in lib.trilbySystem {
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
      (lib.optionalAttrs (!efi) {
        boot.loader = {
          systemd-boot.enable = false;
          efi.canTouchEfiVariables = false;
          grub = {
            enable = true;
            devices = [ "/dev/disk/by-id/virtio-root" ];
          };
        };
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

  allOutputs = p: if p ? outputs then map (o: p.${o}) p.outputs else [ p ];

  evalClosure = pkgs:
    collectInputs (
      lib.attrValues trilbyConfig.inputs
      ++ lib.concatMap lib.attrValues (lib.attrValues releases)
      ++ lib.attrValues (lib.loadFlake { src = ../trilby-cli; }).defaultNix.inputs
    )
    ++ allOutputs pkgs.haskellPackages.cabal2nix-unstable
    ++ map (n: pkgs.haskellPackages.${n}.cabal2nixDeriver)
      [ "trilby-cli" "hnix" "path-io-effectful" "terminal-widgets" "text-rope-zipper" "terminal" ];

  installClosure = pkgs: args:
    evalClosure pkgs
    ++ [ (referenceSystem args).config.system.build.toplevel ]
    ++ (let
      c = (referenceSystem args).config;
      outputsOf = p: lib.concatMap (o: lib.optional (p ? ${o}) p.${o})
        ((p.meta.outputsToInstall or [ "out" ]) ++ c.environment.extraOutputsToInstall);
    in lib.concatMap outputsOf (c.environment.systemPackages))
    ++ allOutputs pkgs.nix-monitored
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

    system.extraDependencies = installClosure pkgs args;
  };

  targetNode = efi: {
    virtualisation = {
      inherit diskImage;
      useBootLoader = true;
      useEFIBoot = efi;
      useDefaultFilesystems = false;
      efi.keepVariables = false;
    };
    virtualisation.fileSystems."/" = {
      device = "/dev/disk/by-label/this-is-not-real-and-will-never-be-used";
      fsType = "ext4";
    };
  };

  mkInstallTest = name: { filesystem ? "ext4", luks ? false, boot ? true, efi ? true }:
    lib.trilbyTest {
      name = "trilby-installer-${name}";
      inherit trilby;
      skipLint = true;
      globalTimeout = 120;
      nodes = {
        installer = installerNode { inherit filesystem luks efi; };
        target = targetNode efi;
      };
      testScript = /*python*/ ''
        installer.start()
        installer.wait_for_unit("multi-user.target")
        installer.succeed("udevadm settle")

        with subtest("Install"):
            installer.succeed("echo hunter2 > /tmp/password")
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
                + " ${if luks then "--luks --luks-password-file /tmp/password" else "--no-luks"}"
                + " --filesystem ${filesystem}"
                + " --boot-mode ${if efi then "efi" else "bios"}"
                + " --edition server --release unstable"
                + " --hostname testhost"
                + " --keyboard us --locale en_GB.UTF-8 --timezone Europe/Zurich"
                + " --username testuser --password-file /tmp/password"
                + " --edit --no-reboot"
                + " > /tmp/install.log 2>&1"
            )
            if status != 0:
                print(installer.execute("grep -a -E 'error|Failed assertions|^- ' /tmp/install.log | grep -a -v -E 'Cannot build|nix-output-monitor' | tail -n 25; tail -n 10 /tmp/install.log")[1])
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

  snakeOil = import "${release.nixpkgs}/nixos/tests/ssh-keys.nix" pkgs;
  snakeOilPublicKey = snakeOil.snakeOilPublicKey;

  infectSystem =
    lib.trilbySystem {
      trilby = { edition = "server"; hostPlatform = buildPlatform; };
      modules = [
        nixosModules.formats.kexecScript
        { users.users.trilby.openssh.authorizedKeys.keys = [ snakeOilPublicKey ]; }
      ];
    };

  infectImage =
    assert infectSystem.config.services.openssh.enable;
    assert infectSystem.config.users.users.trilby.openssh.authorizedKeys.keys == [ snakeOilPublicKey ];
    infectSystem.config.system.build.kexecTree;

  infectInstallArgs = { filesystem = "ext4"; luks = false; efi = true; };

  installBundle = pkgs:
    pkgs.linkFarm "install-closure"
      (lib.imap0 (i: path: { name = toString i; inherit path; }) (installClosure pkgs infectInstallArgs));

  bundleInfo = pkgs: pkgs.closureInfo { rootPaths = [ (installBundle pkgs) ]; };

  mkInfectTest = lib.trilbyTest {
    name = "trilby-infect";
    inherit trilby;
    skipLint = true;
    globalTimeout = 300;
    nodes = {
      controller = { pkgs, ... }: {
        imports = [ nixosModules.profiles.installer ];
        virtualisation.memorySize = 4096;
        nix.settings = {
          substituters = lib.mkForce [ "daemon" ];
          connect-timeout = 1;
          flake-registry = "";
        };
        system.extraDependencies = [ infectImage (installBundle pkgs) (bundleInfo pkgs) ];
        networking.firewall.allowedUDPPorts = [ 67 ];
        services.dnsmasq = {
          enable = true;
          settings = {
            interface = "eth1";
            bind-interfaces = true;
            dhcp-range = [ "192.168.1.100,192.168.1.100,1h" ];
          };
        };
      };
      victim = {
        virtualisation = {
          cores = 4;
          memorySize = 6144;
          diskSize = 16384;
          useBootLoader = true;
          useEFIBoot = true;
        };
        virtualisation.sharedDirectories.hoststore = {
          source = builtins.storeDir;
          target = "/mnt/hoststore";
        };
        services.openssh.settings.PermitRootLogin = "prohibit-password";
        users.users.root.openssh.authorizedKeys.keys = [ snakeOilPublicKey ];
        system.extraDependencies = [ infectImage ];
      };
    };
    testScript = /*python*/ ''
      import shlex

      ip = "192.168.1.100"
      sshopts = "-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

      def on_victim(cmd):
          return f"ssh {sshopts} trilby@{ip} {shlex.quote(cmd)}"

      start_all()
      controller.wait_for_unit("multi-user.target")
      victim.wait_for_unit("multi-user.target")

      controller.succeed("mkdir -p /root/.ssh && chmod 700 /root/.ssh")
      controller.copy_from_host("${snakeOil.snakeOilPrivateKey}", "/root/.ssh/id_ecdsa")
      controller.succeed("chmod 600 /root/.ssh/id_ecdsa")
      controller.wait_until_succeeds("ssh root@victim true")
      victim.succeed("touch /run/not-infected-yet")

      infect = "trilby infect root@victim --edition server --authorised-key '${snakeOilPublicKey}'"

      with subtest("Without --reboot the victim is prepared but keeps running"):
          controller.succeed(infect + " --no-reboot >&2")
          victim.succeed("test -e /run/not-infected-yet")

      with subtest("The controller reboots the victim into the Trilby image"):
          # a real terminal, as a user would have: it matters for processes that outlive the ssh session
          controller.succeed("script -qec " + shlex.quote(infect + " --reboot") + " /tmp/infect.log </dev/null")
          controller.wait_until_succeeds(on_victim("grep -q init=${infectSystem.config.system.build.toplevel}/init /proc/cmdline"))
          assert controller.succeed(on_victim("hostname")).strip() == "trilby"
          controller.succeed(on_victim("test ! -e /run/not-infected-yet"))

      with subtest("The controller installs Trilby onto the victim"):
          controller.copy_from_host("${testOptions}", "/tmp/test-options.nix")
          controller.succeed("printf '%s\\n' '#!/bin/sh' 'cp /tmp/test-options.nix \"$(dirname \"$2\")/test-options.nix\"' > /tmp/editor")
          controller.succeed("printf '%s\\n' 'flake-registry =' 'connect-timeout = 1' > /tmp/extra.conf")
          controller.succeed(f"scp {sshopts} /tmp/editor /tmp/test-options.nix /tmp/extra.conf trilby@{ip}:/tmp/")
          controller.succeed(on_victim("run0 sh -c 'cat /etc/nix/nix.conf /tmp/extra.conf > /tmp/nix.conf; rm /etc/nix/nix.conf; cp /tmp/nix.conf /etc/nix/nix.conf'"))
          controller.succeed(on_victim("chmod +x /tmp/editor; echo hunter2 > /tmp/password"))
          controller.succeed(f"scp {sshopts} ${bundleInfo pkgs}/registration trilby@{ip}:/tmp/registration")
          controller.succeed(on_victim(
              "run0 sh -c 'mkdir -p /run/hoststore /run/store/upper /run/store/work"
              " && mount -t virtiofs hoststore /run/hoststore"
              " && mount -t overlay overlay -o lowerdir=/run/hoststore,upperdir=/run/store/upper,workdir=/run/store/work /nix/store"
              " && nix-store --load-db < /tmp/registration'"
          ))
          install = (
              "EDITOR=/tmp/editor TRILBY_INPUT_OVERRIDES='${inputOverrides}' trilby install"
              " --disk /dev/vda --format --no-luks --filesystem ext4"
              " --edition server --release unstable --hostname testhost"
              " --keyboard us --locale en_GB.UTF-8 --timezone Europe/Zurich"
              " --username testuser --password-file /tmp/password --edit --reboot"
          )
          status, _ = controller.execute(on_victim(install) + " > /tmp/install.log 2>&1")
          if status not in (0, 255) or "Command failed" in controller.succeed("cat /tmp/install.log"):
              print(controller.succeed("tail -n 30 /tmp/install.log"))
              raise Exception("remote installation failed")

      with subtest("The victim reboots into the installed system"):
          victim.wait_for_shutdown()
          victim.start()
          victim.wait_for_unit("multi-user.target")
          assert victim.succeed("hostname").strip() == "testhost"
          assert victim.succeed("id -u testuser").strip() == "1000"
          assert victim.succeed("findmnt -no FSTYPE /").strip() == "ext4"
          victim.succeed("test -f /etc/trilby/flake.nix")
          victim.fail("grep -q init=${infectSystem.config.system.build.toplevel}/init /proc/cmdline")
    '';
  };
in
{
  "installer-ext4" = mkInstallTest "ext4" { };
  "installer-btrfs-luks" = mkInstallTest "btrfs-luks" { filesystem = "btrfs"; luks = true; boot = false; };
  "installer-bios" = mkInstallTest "bios" { efi = false; };
  "infect" = mkInfectTest;
}
