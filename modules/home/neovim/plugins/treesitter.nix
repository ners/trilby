{ pkgs, ... }:

{
  programs.nixvim.plugins.treesitter = {
    enable = true;
    grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      bash
      haskell
      haskell_persistent
      json
      lua
      markdown
    ];
    lazyLoad.settings.event = "DeferredUIEnter";
  };
}
