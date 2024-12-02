{ trilby, pkgs, lib, ... }:

let
  fonts = with pkgs; [
    carlito
    dejavu_fonts
    fira
    fira-code
    fira-mono
    inconsolata
    inter
    libertine
    noto-fonts
    noto-fonts-emoji
    noto-fonts-extra
    roboto
    roboto-mono
    source-code-pro
    source-sans-pro
    source-serif-pro
    twitter-color-emoji
    unstable.corefonts
  ] ++ (
    if (lib.versionAtLeast trilby.release "24.11") then [
      nerd-fonts.cousine
      nerd-fonts.fira-code
      nerd-fonts.iosevka
      nerd-fonts.roboto-mono
      nerd-fonts.sauce-code-pro
    ] else [
      (nerdfonts.override {
        fonts = [
          "Cousine"
          "FiraCode"
          "Iosevka"
          "RobotoMono"
          "SourceCodePro"
        ];
      })
    ]
  );
in
{
  fonts = lib.mkMerge [
    {
      fontDir.enable = true;
      fontconfig.enable = true;
      enableGhostscriptFonts = true;

      fontconfig.defaultFonts = {
        sansSerif = [ "Source Sans Pro" ];
        serif = [ "Source Serif Pro" ];
        monospace = [ "Iosevka Nerd Font" ];
        emoji = [ "Noto Color Emoji" ];
      };

      fontconfig.localConf = /*xml*/ ''
        <?xml version="1.0"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
          <alias binding="weak">
            <family>monospace</family>
            <prefer>
              <family>emoji</family>
            </prefer>
          </alias>
          <alias binding="weak">
            <family>sans-serif</family>
            <prefer>
              <family>emoji</family>
            </prefer>
          </alias>
          <alias binding="weak">
            <family>serif</family>
            <prefer>
              <family>emoji</family>
            </prefer>
          </alias>
        </fontconfig>
      '';
    }
    (if (lib.versionAtLeast trilby.release "23.11")
    then { packages = fonts; }
    else { inherit fonts; }
    )
  ];
}
