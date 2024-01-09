{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = vim-illuminate;
      type = "lua";
      config = /*lua*/ ''
        require('illuminate').configure({})
      '';
    }
  ];
}
