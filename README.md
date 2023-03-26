<p align="center">
	<img height="125" src="assets/Trilby-minimal.svg" alt="Trilby logo">
</p>

Trilby is a NixOS-based operating system that is modeled after Fedora Linux.
It provides new users with sensible defaults and a great out-of-the-box experience.

## Features

- Provides a modern, up-to-date system that is designed for both desktop and server use cases.
- Built on top of NixOS, which is known for its declarative and reproducible nature, making it easy to maintain and upgrade.
- Can be cross-compiled for multiple target architectures, including x86_64, aarch64, and riscv-64.
- Built on top of Nixpkgs which includes a large number of software packages, including popular applications such as Firefox, LibreOffice, and GIMP.
- Designed to be easy to use and install, with an installer that guides you through the process.
- Designed to be easy to extend in your own NixOS configuration.

## Getting Started

To get started with Trilby, you can build the ISO image from this repository and burn it to a USB drive or DVD.
You can then boot from the media and follow the installer prompts to install the operating system.

If you already have NixOS installed, you can add Trilby as a flake input into your NixOS flake, and import its exposed `nixosModules`.

## Building Trilby

If you want to build Trilby from scratch, you can use the provided flake to create a custom ISO image.
To build Trilby, you will need to have Nix installed on your system.

```bash
nix build .#trilby-<edition>-<release>-<arch>-<format>
```

For example, to build the ISO installer for the latest release of the Workstation edition for x86_64:

```bash
nix build .#trilby-workstation-unstable-x86_64-isoImage
```

To build a VirtualBox OVA for the 22.11 stable release of the Server edition for aarch64:

```bash
nix build .#trilby-server-22.11-aarch64-virtualBoxOVA
```

You can find the built images in the `result` directory.

## Contributing

If you want to contribute to Trilby, you can fork the project on GitHub and submit pull requests with your changes.
We welcome contributions of all types, including bug fixes, feature requests, and documentation improvements.

## Licence

Licenced under the Apache Licence, Version 2.0. See the [LICENCE](./LICENCE) file for more details.
