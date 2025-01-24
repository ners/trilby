<p align="center">
	<img height="125" src="assets/Trilby-minimal.svg" alt="Trilby logo">
</p>

Trilby is a NixOS-based operating system that is modeled after Fedora Linux.
It provides new users with sensible defaults and a great out-of-the-box experience.

Additionally, Trilby can be used to get started with great Nix defaults on macOS.

## Installing Trilby on Linux

Boot into any Linux live CD (Ubuntu, NixOS, ...).

Then run the Trilby installer:
```bash
curl -fsSL https://raw.githubusercontent.com/ners/trilby/refs/heads/main/quickstart.sh \
   | sh -s -- install
```

> [!TIP]
> The Trilby installer does not currently support dual-boot or other complex partitioning situations.
>
> You may create the partitions manually before running the installer.
>
> See [the NixOS manual](https://nixos.org/manual/nixos/stable/#sec-installation-manual-partitioning) for more information.

## Installing Trilby on macOS

Run the Trilby installer:
```bash
curl -fsSL https://raw.githubusercontent.com/ners/trilby/refs/heads/main/quickstart.sh \
    | sh -s -- install
```

## Contributing

We welcome contributions of all types, including bug fixes, feature requests, and documentation improvements.

If you wish to dive in and help, look for [issues](/issues) labelled with "good first issue".
