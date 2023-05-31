{ lib, ... }:

self: super: {
  nerdify =
    { name
    , fontfile
    , mono ? false
    }:
    super.runCommand name
      {
        src = super.fetchFromGitHub {
          owner = "ryanoasis";
          repo = "nerd-fonts";
          rev = "v3.0.1";
          hash = "sha256-52gG0jV7TunD/MoTxSSkXniLW5/X1pHwQwzK05TCMBE=";
        };
        buildInputs = with super; [
          argparse
          fontforge
          (python3.withPackages (ps: with ps; [ setuptools fontforge ]))
        ];
      } ''
      python3 \
        "$src/font-patcher" \
        --complete \
        --outputdir $out/share/fonts/${
          if lib.hasSuffix ".otf" fontfile
              then "opentype"
              else "truetype"
        } \
        ${if mono then "--mono" else ""} \
        "${fontfile}"
    '';
}
