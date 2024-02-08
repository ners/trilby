{ nixosConfig, ... }:

{
  programs.firefox = {
    enable = true;
    inherit (nixosConfig.programs.firefox) package;
  };
}
