{ ... }:

self: super: {
  termify = { name, fontfile, dpi ? 100, ptSize ? 10, extraWidth ? 9, ... }:
    let
      bdf2psf-data = "${super.bdf2psf}/share/bdf2psf";
      inherit (super.lib) hasSuffix;
    in
    super.runCommand name
      {
        buildInputs = with super; [ otf2bdf bdf2psf gzip ];
        sets = super.lib.concatStringsSep "+" (map (x: "${bdf2psf-data}/${x}") [
          "ascii.set"
          "linux.set"
          "fontsets/Lat2.256"
          "fontsets/Uni1.512"
          "useful.set"
        ]);
      } ''
      ${if hasSuffix ".ttf" fontfile || hasSuffix ".otf" fontfile
        then "otf2bdf ${fontfile} -r ${toString dpi} -p ${toString ptSize} -o tmp.bdf || true"
        else if hasSuffix ".bdf" fontfile
          then "cp ${fontfile} tmp.bdf"
          else throw "termify: unrecognised font format: ${fontfile}"
      }

      if ! grep -q AVERAGE_WIDTH tmp.bdf; then
        sed -i 's,POINT_SIZE \(.*\),&\nAVERAGE_WIDTH \1,' tmp.bdf
      fi

      AV=$( sed -n 's,AVERAGE_WIDTH ,,p' tmp.bdf )
      AV=$(( ( AV + ${builtins.toString extraWidth} ) / 10 * 10 ))
      sed -i "/AVERAGE_WIDTH/s, .*, $AV," tmp.bdf

      bdf2psf --fb tmp.bdf ${bdf2psf-data}/standard.equivalents $sets 512 - | gzip -k - > $out
    '';
}
