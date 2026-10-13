{ trilby, lib, pkgs, ... }:

{
  system.activationScripts.binsh = lib.mkForce ""; # obsolete

  system.activationScripts.usrbinenv = lib.mkForce /*bash*/ ''
    sw=/run/current-system/sw

    link() {
      mkdir -p "$(dirname "$2")"
      if [ -d "$2" ] && [ ! -L "$2" ]; then rm -rf "$2"; fi
      ln -sfn "$1" "$2"
    }
    link $sw/bin /bin
    link $sw/bin /usr/bin
    link $sw/bin /usr/local/bin
    link $sw/share /usr/share

    for dir in /lib /lib64; do
      if [ -L "$dir" ]; then rm "$dir"; fi
      mkdir -p "$dir"
      find "$dir" -maxdepth 1 -type l -lname "$sw/lib/*" -delete
      for f in "$systemConfig"/sw/lib/*; do
        if [ -e "$f" ]; then ln -sfn "$sw/lib/$(basename "$f")" "$dir/$(basename "$f")"; fi
      done
    done
  '';

  environment = {
    ldso = pkgs.stdenv.cc.bintools.dynamicLinker;
  } // lib.optionalAttrs (trilby.hostSystem.cpu.name == "x86_64") {
    ldso32 = pkgs.pkgsi686Linux.stdenv.cc.bintools.dynamicLinker;
  };
}
