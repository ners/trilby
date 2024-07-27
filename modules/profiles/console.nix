{ config, lib, pkgs, ... }:

let
  cozette-psf = pkgs.termify {
    name = "cozette.psf.gz";
    fontfile = "${pkgs.cozette}/share/fonts/misc/cozette.bdf";
    scale = config.console.fontScale;
  };
in
{
  options.console.fontScale = with lib; mkOption {
    type = types.int;
    default = 1;
  };

  config = {
    boot.kernelParams = [ "console=ttyS0" "console=tty1" ];

    console = {
      # Enable setting virtual console options as early as possible (in initrd).
      earlySetup = true;
      useXkbConfig = true;
      font = cozette-psf;
    };
  };
}
