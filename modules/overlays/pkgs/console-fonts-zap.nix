{ lib
, stdenvNoCC
, fetchzip
, ...
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "console-fonts-zap";
  version = "2.4";
  src = fetchzip {
    url = "https://ftp.zap.org.au/pub/fonts/console-fonts-zap/console-fonts-zap-${finalAttrs.version}.tar.xz";
    hash = "sha256-vB3qQdLuLT6/mm1pHi08RXyqxQEEyjtgR3cY1/RDMF0=";
  };
  dontConfigure = true;
  dontBuild = true;
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/consolefonts"
    cp *.psf *.psftx "$out/share/consolefonts"
    runHook postInstall
  '';
  meta = {
    description = "The Linux Console Fonts from The ZAP Group Australia package contains two varieties of Linux console fonts: one set that mimics the traditional VGA fonts, and a second that provides a lighter, easier to read look. Both font varieties contain a somewhat more useful set of characters than traditional console fonts, and are available in versions with either 256 or 512 glyphs.";
    homepage = "https://www.zap.org.au/projects/console-fonts-zap/";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.all;
    maintainers = with lib.maintainers; [ ners ];
  };
})
