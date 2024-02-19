import os

trilby.start()

trilby.wait_for_unit('default.target')

trilby.succeed(" ".join([
    'trilby',
    '--verbosity debug',
    'install',
    '--format',
    '--disk /dev/vdb',
    '--luks --luks-password foo',
    '--filesystem btrfs',
    '--edition server',
    '--channel unstable',
    '--hostname testhost',
    '--keyboard us',
    '--locale en_GB.UTF-8',
    '--timezone Europe/Zurich',
    '--username testuser',
    '--password foo',
    '--no-reboot',
    '| logger'
]))

trilby.succeed('sync')
trilby.succeed('cryptsetup isLuks /dev/vdb3');
trilby.succeed('findmnt /mnt')
trilby.succeed('findmnt /mnt/home')
trilby.succeed('findmnt /mnt/nix')

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
