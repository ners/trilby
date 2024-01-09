{ pkgs, ... }:

{
  programs.neovim.plugins = [
    {
      plugin = pkgs.vimPlugins.nvim-base16;
      type = "viml";
      config = /*vim*/ ''
        colorscheme base16-default-dark
      '';
    }
  ];
}
