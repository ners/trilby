{ pkgs, ... }:

{
  boot.kernelParams = [
    "console=ttyS0"
    "console=tty1"
  ];

  console = {
    # Enable setting virtual console options as early as possible (in initrd).
    earlySetup = true;
    useXkbConfig = true;

    font = "${pkgs.unstable.console-setup}/share/consolefonts/Uni3-Fixed16.psf.gz";
  };
}
