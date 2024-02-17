{ nixosConfig, ... }:

{
  programs.firefox = {
    inherit (nixosConfig.programs.firefox) enable package;
  };
}
