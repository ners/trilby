{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = nvim-hlslens;
      type = "lua";
      config = /*lua*/ ''
        require("hlslens").setup()
      '';
    }
  ];
}
