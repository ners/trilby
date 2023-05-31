{ pkgs, ... }:

{
  programs.neovim.plugins = [
    {
      plugin = pkgs.vimPlugins.nvim-autopairs;
      type = "lua";
      config = ''
        require('nvim-autopairs').setup({})
      '';
    }
  ];
}
