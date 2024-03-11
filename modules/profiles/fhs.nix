{ lib, ... }:

{
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
    copyLinks /run/current-system/sw/bin /bin /usr/bin /usr/local/bin
    copyLinks /run/current-system/sw/lib /lib /lib64
  '';
}
