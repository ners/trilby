_: final: prev:
let
  inherit (prev) lib;
in
lib.optionalAttrs (lib.versionAtLeast lib.trivial.release "26.11") {
  libguestfs = prev.libguestfs.overrideAttrs (attrs: {
    version = "1.60.0";
    src = attrs.src.overrideAttrs {
      outputHash = "sha256-dfr1Ypy3m+NYML63oNGBgC0WedN4ElTRPq1ZUcgfYQs=";
    };
    meta = attrs.meta // {
      broken = false;
    };
  });

  libguestfs-appliance = prev.libguestfs-appliance.overrideAttrs (finalAttrs: prevAttrs: {
    version = "1.58.1";
    src = final.fetchurl {
      url = "https://download.libguestfs.org/binaries/appliance/appliance-${finalAttrs.version}.tar.xz";
      hash = "sha256-Z3nOP8ggoowWB3KYkuU6G6QYvkSA9S4EV7s4HqtAinI=";
    };
    passthru = prevAttrs.passthru or { } // {
      libguestfsCompatible = _: true;
    };
  });
}
