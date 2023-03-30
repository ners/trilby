{ pkgs, ... }:

{
  programs.neovim.plugins = [
    (pkgs.unstable.vimPlugins.nvim-treesitter.withPlugins (_: pkgs.unstable.tree-sitter.allGrammars))
  ];
}
