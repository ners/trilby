#!/bin/sh
set -eu

# Ensure Nix
if ! command -v nix >/dev/null 2>&1; then
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

# Ensure flakes
if ! nix flake metadata nixpkgs >/dev/null 2>&1; then
    mkdir -p ~/.config/nix
    echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
fi

# Ensure nom
case "$(basename $(realpath $(command -v nix)))" in
    "nix-monitored")
        ;;
    "nix")
        export PATH="$(nix build --accept-flake-config --no-link --print-out-paths github:ners/trilby#nix-monitored.out):$PATH"
        ;;
    *)
        echo "error: unexpected output of command -v nix" >&2
        exit 1
        ;;
esac

nix run --accept-flake-config github:ners/trilby -- "$@"
