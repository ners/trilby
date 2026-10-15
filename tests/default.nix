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
      users.users.root.initialHashedPassword = lib.mkForce null;
    }
  '';

  referenceSystem = { filesystem, luks, efi }: let rootDevice = if luks then "/dev/mapper/cryptroot" else "/dev/vda3"; in lib.trilbySystem {
    inherit trilby;
    modules = [
      ({ trilby, inputs, lib, ... }: {
        imports = [ "${testOptions}" (lib.trilbyUser trilby { name = "testuser"; initialPassword = "hunter2"; inherit inputs; }) ];
        networking.hostName = "testhost";
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
      always-allow-substitutes = true;
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
                "EDITOR=/tmp/editor "
                + "trilby --debug --offline install"
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

  mkOfflineTest = lib.trilbyTest {
    name = "trilby-offline";
    inherit trilby;
    skipLint = true;
    globalTimeout = 120;
    nodes.machine = { };
    testScript = ''
      machine.wait_for_unit("multi-user.target")

      for command in ["update", "clean", "infect"]:
          with subtest(f"{command} refuses to operate on a remote host"):
              status, out = machine.execute(f"trilby --offline {command} root@remote 2>&1")
              assert status != 0, out
              assert "--offline forbids operating on remote host" in out, out

      with subtest("Offline mode refuses to install Nix"):
          status, out = machine.execute("PATH=/nonexistent $(command -v trilby) --offline clean 2>&1")
          assert status != 0, out
          assert "Nix is required but not installed" in out, out
          assert "Download" not in out, out
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
          controller.succeed(f"scp {sshopts} /tmp/editor /tmp/test-options.nix trilby@{ip}:/tmp/")
          controller.succeed(on_victim("chmod +x /tmp/editor; echo hunter2 > /tmp/password"))
          controller.succeed(f"scp {sshopts} ${bundleInfo pkgs}/registration trilby@{ip}:/tmp/registration")
          controller.succeed(on_victim(
              "run0 sh -c 'mkdir -p /run/hoststore /run/store/upper /run/store/work"
              " && mount -t virtiofs hoststore /run/hoststore"
              " && mount -t overlay overlay -o lowerdir=/run/hoststore,upperdir=/run/store/upper,workdir=/run/store/work /nix/store"
              " && nix-store --load-db < /tmp/registration"
              " && mkdir -p /root/.config/nix"
              " && echo always-allow-substitutes = true > /root/.config/nix/nix.conf'"
          ))
          install = (
              "EDITOR=/tmp/editor trilby --offline install"
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

  configFlake = pkgs.writeText "flake.nix" /*nix*/ ''
    {
      inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
        home-manager.url = "github:nix-community/home-manager";
        nixvim = {
          url = "github:nix-community/nixvim";
          inputs.nixpkgs.follows = "nixpkgs";
        };
        trilby = {
          url = "github:ners/trilby";
          inputs = {
            nixpkgs.follows = "nixpkgs";
            nixpkgs-unstable.follows = "nixpkgs";
            home-manager.follows = "home-manager";
            nixvim.follows = "nixvim";
          };
        };
      };
      outputs = inputs:
        let inherit (inputs.trilby) lib; in
        {
          inherit (inputs.trilby) legacyPackages;
          nixosConfigurations =
            lib.mapAttrs (_: host: import host { inherit inputs lib; }) (lib.findModules ./hosts);
        };
    }
  '';

  configLockArgs = lib.concatMapStringsSep " "
    (name: "--override-input ${name} path:${release.${name}}")
    [ "nixpkgs" "home-manager" "nixvim" ]
  + " --override-input trilby trilby";

  mkConfigTest = lib.trilbyTest {
    name = "trilby-config";
    inherit trilby;
    skipLint = true;
    globalTimeout = 300;
    nodes.machine = { pkgs, ... }: {
      virtualisation = {
        cores = 4;
        memorySize = 4096;
      };
      nix.settings.substituters = lib.mkForce [ ];
      system.extraDependencies = evalClosure pkgs;
    };
    testScript = /*python*/ ''
      import json

      def config(args):
          status, out = machine.execute(f"USER=root trilby --offline config {args} 2>/tmp/stderr")
          print(machine.succeed("cat /tmp/stderr"))
          if status != 0:
              raise Exception(f"trilby config {args} exited with {status}")
          return out

      def listing(args):
          return json.loads(config(args + " --json"))

      def hosts_of(entry):
          return sorted(scope["host"] for scope in entry["scopes"])

      machine.wait_for_unit("multi-user.target")

      with subtest("A flake can be created and locked offline"):
          machine.succeed("mkdir -p /etc/trilby/hosts")
          machine.copy_from_host("${configFlake}", "/etc/trilby/flake.nix")
          machine.succeed("cd /etc/trilby && NIX_CONFIG='flake-registry =' nix --offline flake lock ${configLockArgs}")

      with subtest("A host can be added"):
          config("hosts add alpha --edition server --platform ${buildPlatform}")
          machine.succeed("cp -r /etc/trilby/hosts/alpha /etc/trilby/hosts/beta")
          for host in ["alpha", "beta"]:
              machine.succeed(f"echo '{{ documentation.enable = false; }}' > /etc/trilby/hosts/{host}/options.nix")
          machine.succeed(
              "cat > /etc/trilby/hosts/alpha/filesystems.nix <<'EOF'\n"
              + "{\n"
              + "  fileSystems.\"/\" = { device = \"/dev/vda1\"; fsType = \"ext4\"; };\n"
              + "  fileSystems.\"/boot\" = { device = \"/dev/vda2\"; fsType = \"vfat\"; };\n"
              + "}\n"
              + "EOF"
          )

      with subtest("Packages and services are added to a host"):
          config("packages add cowsay --host alpha --no-action")
          config("services add nginx --host alpha --no-action")
          configuration = machine.succeed("cat /etc/trilby/hosts/alpha/configuration.nix")
          assert "cowsay" in configuration, configuration
          assert "services.nginx.enable = true" in configuration, configuration
          machine.fail("grep -q cowsay /etc/trilby/hosts/beta/configuration.nix")

      with subtest("Packages are listed once, by store path"):
          packages = listing("packages list --host alpha,beta --system --unique --full-path")
          assert all(p["name"].startswith("/nix/store/") for p in packages), packages[:3]
          [cowsay] = [p for p in packages if "-cowsay-" in p["name"]]
          assert hosts_of(cowsay) == ["alpha"], cowsay
          assert cowsay["origins"] == ["local"], cowsay

      with subtest("Hosts can be compared"):
          changes = {(c["kind"], c["name"]) for c in listing("diff alpha beta --kind packages,services")}
          assert ("services", "nginx") in changes, changes
          assert any(kind == "packages" and name.startswith("cowsay") for kind, name in changes), changes

      with subtest("Entries can be searched"):
          found = listing("search nginx --system --kind services")
          assert [(e["kind"], e["name"]) for e in found] == [("services", "nginx")], found
          assert hosts_of(found[0]) == ["alpha"], found

      with subtest("A host can be edited"):
          machine.succeed("printf '#!/bin/sh\\necho \"# edited\" >> \"$1\"\\n' > /tmp/editor && chmod +x /tmp/editor")
          machine.succeed("VISUAL=/tmp/editor USER=root trilby --offline config edit --host alpha --no-action 2>/tmp/stderr")
          machine.succeed("grep -q '# edited' /etc/trilby/hosts/alpha/configuration.nix")

      with subtest("Packages are removed again"):
          config("packages rm cowsay --host alpha --yes --no-action")
          machine.fail("grep -q cowsay /etc/trilby/hosts/alpha/configuration.nix")

      with subtest("Hosts are checked for evaluation errors"):
          status, out = machine.execute("USER=root trilby --offline config check --host alpha,beta 2>/dev/null")
          assert status != 0, out
          assert any(line.startswith("ok") and "alpha" in line for line in out.splitlines()), out
          assert any(line.startswith("FAIL") and "beta" in line and "root file system" in line for line in out.splitlines()), out
    '';
  };
in
{
  "installer-ext4" = mkInstallTest "ext4" { };
  "installer-btrfs-luks" = mkInstallTest "btrfs-luks" { filesystem = "btrfs"; luks = true; boot = false; };
  "installer-bios" = mkInstallTest "bios" { efi = false; };
  "offline" = mkOfflineTest;
  "infect" = mkInfectTest;
  "config" = mkConfigTest;
}
