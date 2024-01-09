{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = nvim-scrollbar;
      type = "lua";
      config = /*lua*/ ''
        require("scrollbar").setup()
        require("scrollbar.handlers.search").setup()
      '';
    }
  ];
}
