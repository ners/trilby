{ lib
, stdenvNoCC
, fetchzip
, perl
, genericUpdater
, writeShellScript
, ...
}:

stdenvNoCC.mkDerivation rec {
  pname = "console-setup";
  version = "1.233";

  # Git repo: https://salsa.debian.org/installer-team/console-setup.git
  # But it seems to be offline
  src = fetchzip {
    url = "https://sources.debian.org/debian/pool/main/c/console-setup/console-setup_${version}.tar.xz";
    hash = "sha256-r0QJs97cTPyOcRbMKDawfQnqFNS7wq5gJBLAg+aipDk=";
  };

  buildInputs = [ perl ];

  makeFlags = [ "prefix=${placeholder "out"}" ];

  enableParallelBuilding = true;

  postPatch = ''
    patchShebangs .
  '';

  buildTargets = ["install-linux"];

  installTargets = ["install-linux"];

  passthru.updateScript = genericUpdater {
    versionLister = writeShellScript "console-setup-versionLister" ''
      curl -Ls https://sources.debian.org/api/src/console-setup/ | jq --raw-output '.versions.[] | .version'
    '';
  };

  meta = {
    description = "Console font and keymap setup program";
    homepage = "https://tracker.debian.org/pkg/console-setup";
    license = lib.licenses.gpl2;
    platforms = lib.platforms.all;
    maintainers = with lib.maintainers; [ ners ];
  };
}
