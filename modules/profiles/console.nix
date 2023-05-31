{ pkgs, ... }:

{
  console = {
    # Enable setting virtual console options as early as possible (in initrd).
    earlySetup = true;
    useXkbConfig = true;

    font = pkgs.termify {
      name = "cozette.psf.gz";
      fontfile = "${pkgs.cozette}/share/fonts/misc/cozette.bdf";
    };
  };
}
