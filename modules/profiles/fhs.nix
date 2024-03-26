{ lib, ... }:

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
    sw=/nix/var/nix/gcroots/current-system/sw
    copyLinks $sw/bin /bin /usr/bin /usr/local/bin
    copyLinks $sw/lib /lib /lib64
  '';
}
