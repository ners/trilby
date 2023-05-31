{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    nvim-hlslens
    {
      plugin = nvim-scrollbar;
      type = "lua";
      config = ''
        require("scrollbar").setup()
        require("hlslens").setup()
        require("scrollbar.handlers.search").setup()
      '';
    }
  ];
}
