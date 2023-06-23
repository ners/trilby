{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = pkgs.vimPlugins.nvim-treesitter.withPlugins (_:
      pkgs.tree-sitter.allGrammars
    );
      type = "lua";
      config = builtins.readFile ./config.lua;
    }
    nvim-ts-rainbow2
    nvim-treesitter-textobjects
    nvim-treesitter-refactor
  ];
}
