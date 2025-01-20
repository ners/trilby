{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.unstable.vimPlugins; [
    {
      plugin = nvim-treesitter.withAllGrammars;
      type = "lua";
      config = builtins.readFile ./config.lua;
    }
    rainbow-delimiters-nvim
  ];
}
