{ lib, ... }:

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
    };
  };
}
