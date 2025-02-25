#!/usr/bin/env nix
#! nix shell .#bash .#nix-output-monitor .#nvd --command bash
set -ex

if [ -e result ]; then
    rm -f result-prev
    mv result result-prev
fi

find -name flake.lock -execdir nix flake update \;

nom build --keep-going .#trilby-all-toplevel

if [ -e result-prev ]; then
    for f in result-prev/*; do
        nvd diff "$f" result/$(basename "$f")
    done
fi
