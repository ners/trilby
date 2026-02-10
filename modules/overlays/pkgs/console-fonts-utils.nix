{ lib
, stdenvNoCC
, fetchgit
, python3
, ...
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "console-fonts-utils";
  version = "unstable-2025-11-29";
  src = fetchgit {
    url = "https://git.zap.org.au/git/console-fonts-utils.git";
    rev = "06c5d5cf8bce9a3246782ef537f7d737145ff436";
    hash = "sha256-5dmQi2YWaxrPrCtQ6/vgHvYgYJdY7aGWuQ2Q3RovOW0=";
  };
  dontConfigure = true;
  dontBuild = true;
  buildInputs = [ (python3.withPackages (ps: [ ps.reportlab ps.pillow ])) ];
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin"
    for f in *psftx* *.py; do
      install -Dm755 $f "$out/bin"
    done
    patchShebangs "$out/bin"
    runHook postInstall
  '';
  meta = {
    description = "The Linux Console Font Utilities package contains various applications to manipulate Linux console fonts. In particular, programs are included to convert standard PSF font files to a text representation and vice versa, allowing easy font editing using any text editor.";
    homepage = "https://www.zap.org.au/projects/console-fonts-utils/";
    license = lib.licenses.gpl2Only;
    platforms = lib.platforms.all;
    maintainers = with lib.maintainers; [ ners ];
  };
})
