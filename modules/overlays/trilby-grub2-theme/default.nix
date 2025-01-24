{ ... }:

self: super: {
  trilby-grub2-theme = self.runCommand "trilby-grub2-theme" { } ''
    cp -r ${super.nixos-grub2-theme} $out
    chmod +x $out
    chmod +w -R $out
    ${super.lib.getExe' super.librsvg "rsvg-convert"} \
      --height 100 \
      --keep-aspect-ratio \
      ${../../../assets/Trilby-minimal.svg} \
      > $out/logo.png
  '';
}
