{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.unstable.vimPlugins; [
    lsp_extensions-nvim
    lsp_signature-nvim
    lspkind-nvim
    lsp_lines-nvim
    lsp-inlayhints-nvim
  ];
}
