import os

trilby.start()

trilby.wait_for_unit('default.target')

trilbyFlake = trilby.succeed('nix flake metadata --json trilby | jq --raw-output .path').strip()

trilby.succeed(" ".join([
    'cp -r',
    f'{trilbyFlake}/outputs/configuration/isoImage/test/flake-install',
    '/tmp/testFlake'
]))

trilby.succeed(" ".join([
    'trilby',
    '--verbosity debug',
    'install',
    '--flake',
    '/tmp/testFlake#test',
    '--copy-flake',
    '--format',
    '--no-reboot',
    '| logger'
]))

trilby.succeed('sync')
trilby.succeed('findmnt /mnt')
actual = trilby.succeed('nix flake show --json /mnt/etc/trilby | jq --raw-output .nixosConfigurations.test.type').strip()
expected = "nixos-configuration"
assert expected == actual, f"NixOS configuration type is {actual}, expected {expected}"

trilby.shutdown()

trilby = create_machine({
    "name": "trilby",
    "hdaInterface": "virtio",
    "hda": os.path.join(trilby.state_dir, "empty0.qcow2"),
    "qemuFlags": "-cpu max",
})

trilby.start()

trilby.wait_for_unit('default.target')

trilby.succeed('trilby --help')
