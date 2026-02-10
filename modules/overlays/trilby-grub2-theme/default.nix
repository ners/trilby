{ inputs, ... }:

self: super: {
  trilby-grub2-theme =
    self.runCommand "trilby-grub2-theme"
      { nativeBuildInputs = with super; [ librsvg ]; }
      ''
        cp -r ${super.nixos-grub2-theme} $out
        chmod +x $out
        chmod +w -R $out
        rsvg-convert \
          --height 100 \
          --keep-aspect-ratio \
          --output "$out"/logo.png \
          "${inputs.self}"/assets/logo.svg
        rsvg-convert --output "$out"/bios-boot.png "${./.}"/bios-boot.svg
      '';
}
