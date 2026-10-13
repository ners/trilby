{ lib, options, pkgs, ... }:

{
  plugins = lib.mkMerge [
    (lib.optionalAttrs (options.plugins ? haskell-tools) {
      haskell-tools = {
        enable = true;
        hlsPackage = null;
      };
    })
    {
      treesitter.grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
        haskell
        haskell_persistent
      ];
    }
  ];
  files."ftplugin/haskell.lua" = {
    opts = rec {
      expandtab = true;
      tabstop = 4;
      shiftwidth = tabstop;
      softtabstop = tabstop;
    };
    extraConfigLua = ''
      vim.treesitter.start()
    '';
  };
}
