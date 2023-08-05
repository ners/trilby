{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = nvim-treesitter.withAllGrammars;
      type = "lua";
      config = builtins.readFile ./config.lua;
    }
    nvim-ts-rainbow2
    nvim-treesitter-textobjects
    nvim-treesitter-refactor
    {
      plugin = nvim-treesitter-context;
      type = "lua";
      config = "require('treesitter-context').setup({})";
    }
  ];
}
