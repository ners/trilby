{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    lsp-inlayhints-nvim
    lsp_extensions-nvim
    lsp_lines-nvim
    lsp_signature-nvim
    lspkind-nvim
    nvim-lspconfig
  ];
}
