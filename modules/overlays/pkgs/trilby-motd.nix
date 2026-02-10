{ inputs
, runCommand
, jp2a
, artem
, ascii-image-converter
, librsvg
, console-fonts-utils
, console-fonts-zap
}:

let
trilby-motd = runCommand "trilby-motd" { nativeBuildInputs = [ artem ascii-image-converter jp2a librsvg ]; } ''
  rsvg-convert --width 2000 --output logo.png "${inputs.self}"/assets/logo.svg
  jp2a --size=100x13 logo.png > "$out"
  artem --characters '|+ ' --size 120 --no-color logo.png > "$out"
  ascii-image-converter --dimensions 90,10 --map '  +++##' logo.png > "$out"
'';
in
trilby-motd // {
  screenshot = runCommand "trilby-motd.png" { nativeBuildInputs = [ console-fonts-utils ]; } ''
    psftx-screenshot "${console-fonts-zap}"/share/consolefonts/zap-light20.psftx "${trilby-motd}" "$out"
  '';
}
