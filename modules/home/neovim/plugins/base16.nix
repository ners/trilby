{ pkgs, ... }:

{
  programs.neovim.plugins = [
    {
      plugin = pkgs.unstable.vimPlugins.base16-nvim;
      type = "viml";
      config = /*vim*/ ''
        colorscheme base16-default-dark
      '';
    }
  ];
}
