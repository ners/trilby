{ ... }:

self: super: {
  trilby-grub2-theme = self.runCommand "trilby-grub2-theme" { } ''
    cp -r ${super.nixos-grub2-theme} $out
    chmod +x $out
    chmod +w -R $out
    ${super.lib.getExe' super.librsvg "rsvg-convert"} ${../../../assets/Trilby-minimal.svg} -geometry x100 $out/logo.png
  '';
}
