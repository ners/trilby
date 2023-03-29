{ inputs, ... }:

self: super: {
  trilby-grub2-theme = self.runCommand "trilby-grub2-theme"
    {
      buildInputs = with super; [ imagemagick ];
    } ''
    cp -r ${super.nixos-grub2-theme} $out
    chmod +x $out
    chmod +w -R $out
    convert ${../../assets/Trilby.png} -geometry x100 $out/logo.png
  '';
}
