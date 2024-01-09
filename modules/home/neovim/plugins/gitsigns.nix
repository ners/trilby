{ pkgs, ... }:

{
  programs.neovim.plugins = [
    {
      plugin = pkgs.vimPlugins.gitsigns-nvim;
      type = "lua";
      config = /*lua*/ ''
        require('gitsigns').setup({})
      '';
    }
  ];
}
