{ trilby, lib, pkgs, ... }:

{
  system.activationScripts.binsh = lib.mkForce ""; # obsolete

  system.activationScripts.usrbinenv = lib.mkForce /*bash*/ ''
    function copyLinks() {
      src=$1
      shift
      for dst in "$@"; do
        mkdir -p $(dirname $dst)
        rm -rf $dst
        if [ -d $src ]
          then cp -r $src $dst
          else ln -s $src $dst
        fi
      done
    }
    sw=/run/current-system/sw
    copyLinks $sw/bin /bin /usr/bin /usr/local/bin
    copyLinks $sw/lib /lib /lib64
  '';

  environment = lib.mkMerge (
    lib.optionals (lib.versionAtLeast trilby.release "24.05") [
      {
        ldso = "${pkgs.stdenv.cc.libc_lib}/lib64/ld-linux-x86-64.so.2";
      }
      (lib.optionalAttrs (trilby.hostSystem.cpu.name == "x86_64") {
        ldso32 = "${pkgs.stdenv.cc.libc_lib}/lib/ld-linux-x86-64.so.2";
      })
    ]
  );
}
// lib.optionalAttrs (lib.versionAtLeast trilby.release "24.05") {
  environment = {
    ldso = "${pkgs.stdenv.cc.libc_lib}/lib64/ld-linux-x86-64.so.2";
    ldso32 = "${pkgs.stdenv.cc.libc_lib}/lib/ld-linux-x86-64.so.2";
  };
}
