{ lib, pkgs, ... }:

let
  basic = { extraConfigLua = "vim.treesitter.start()"; };
  tabs = { opts.expandtab = false; };
  spaces = { opts.expandtab = true; };
  tabstop = tabstop: {
    opts = {
      inherit tabstop;
      softtabstop = tabstop;
      shiftwidth = tabstop;
    };
  };
  ftplugin = lang: plugin: { "ftplugin/${lang}.lua" = lib.mkMerge plugin; };
in
{
  programs.nixvim = {
    plugins.treesitter = {
      enable = true;
      grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
        bash
        json
        lua
        markdown
        python
        yaml
      ];
    };
    files = lib.mkMerge [
      (ftplugin "bash" [ basic tabs (tabstop 4) ])
      (ftplugin "json" [ basic tabs ])
      (ftplugin "lua" [ basic tabs (tabstop 4) ])
      (ftplugin "markdown" [ basic spaces ])
      (ftplugin "python" [ basic spaces (tabstop 4) ])
      (ftplugin "yaml" [ basic spaces (tabstop 2) ])
    ];
  };
}

