{ pkgs, ... }:

{
  programs.neovim.plugins = [
    {
      plugin = pkgs.vimPlugins.base16-nvim;
      type = "viml";
      config = /*vim*/ ''
        colorscheme base16-default-dark
      '';
    }
  ];
}
