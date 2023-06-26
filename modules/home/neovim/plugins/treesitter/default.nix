{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.unstable.vimPlugins; [
    {
      plugin = pkgs.unstable.vimPlugins.nvim-treesitter.withPlugins (_:
        pkgs.unstable.tree-sitter.allGrammars
      );
      type = "lua";
      config = builtins.readFile ./config.lua;
    }
    nvim-ts-rainbow2
    nvim-treesitter-textobjects
    nvim-treesitter-refactor
  ];
}
