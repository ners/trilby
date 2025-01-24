{ pkgs, ... }:

{
  programs.nixvim = {
    files."ftplugin/nix.lua" = {
      opts = rec {
        expandtab = true;
        tabstop = 2;
        shiftwidth = tabstop;
        softtabstop = tabstop;
      };
      extraConfigLua = /*lua*/ ''
        if vim.fn.executable('nixd') == 1 then
          vim.lsp.start {
            name = 'nixd';
            cmd = { 'nixd' };
            root_dir = vim.fs.dirname(vim.fs.find({'flake.nix', '.git'}, {upward = true})[1]),
            capabilities = vim.lsp.protocol.make_client_capabilities(),
          };
        end
      '';
    };
    plugins.treesitter.grammarPackages = [
      pkgs.vimPlugins.nvim-treesitter.builtGrammars.nix
    ];
  };
}
