{ ... }:

{
  system.activationScripts.fhsSetup = /*bash*/ ''
    for d in /bin /usr/bin /usr/local/bin; do
      mkdir -p $(dirname $d)
      rm -rf $d
      ln -s /run/current-system/sw/bin $d
    done
    for d in /lib /lib64; do
      mkdir -p $(dirname $d)
      rm -rf $d
      ln -s /run/current-system/sw/lib $d
    done
  '';
}
