{ trilby, lib, ... }@attrs:

lib.recursiveConcat [
  (import ./test attrs)
  (import ./qemu.nix (attrs // { name = trilby.configurationName; }))
  (lib.foreach [ "gzip" "lzo" "lz4" "xz" "zstd" ] (import ./compression.nix attrs))
]
