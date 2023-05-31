{ pkgs, ... }:

{
  programs.neovim.plugins = [
    (pkgs.vimPlugins.nvim-treesitter.withPlugins (_:
      pkgs.tree-sitter.allGrammars
    ))
  ];
}
