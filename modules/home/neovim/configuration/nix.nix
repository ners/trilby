{ pkgs, ... }:

{
  plugins.treesitter = {
    grammarPackages = with pkgs.vimPlugins.nvim-treesitter.builtGrammars; [
      nix
    ];
  };
  files."ftplugin/nix.lua" = {
    opts = rec {
      tabstop = 2;
      softtabstop = tabstop;
      shiftwidth = tabstop;
    };
    extraConfigLua = ''
      vim.treesitter.start()
      if vim.fn.executable('nixd') == 1 then
        vim.lsp.start {
          name = 'nixd',
          cmd = { 'nixd' },
          root_dir = vim.fs.dirname(vim.fs.find({'flake.nix', '.git'}, {upward = true})[1]),
          capabilities = vim.lsp.protocol.make_client_capabilities(),
        }
      end
    '';
  };
}
