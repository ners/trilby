{ lib, pkgs, ... }:

{
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
  files =
    let
      tabs = { opts.expandtab = false; };
      spaces = { opts.expandtab = true; };
      tabstop = tabstop: {
        opts = {
          inherit tabstop;
          softtabstop = tabstop;
          shiftwidth = tabstop;
        };
      };
    in
    lib.foreach
      {
        bash = tabs // tabstop 4;
        json = tabs // tabstop 4;
        lua = tabs // tabstop 4;
        markdown = spaces // tabstop 2;
        python = spaces // tabstop 4;
        yaml = spaces // tabstop 2;
      }
      (lang: attrs: {
        "ftplugin/${lang}.lua" = {
          extraConfigLua = lib.mkBefore "vim.treesitter.start()";
        } // attrs;
      });
}
