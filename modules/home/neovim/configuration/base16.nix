{ lib, ... }:

{
  colorschemes.base16 = {
    enable = lib.mkDefault true;
    autoLoad = true;
    colorscheme = lib.mkDefault "default-dark";
  };
}
