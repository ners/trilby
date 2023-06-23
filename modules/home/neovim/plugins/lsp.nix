{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    lsp-inlayhints-nvim
    lsp_extensions-nvim
    lsp_signature-nvim
    lspkind-nvim
    nvim-lspconfig
    {
      plugin = lsp_lines-nvim;
      type = "lua";
      config = ''
        -- Disable virtual_text since it's redundant due to lsp_lines.
        vim.diagnostic.config({
          virtual_text = false,
        })

        require("lsp_lines").setup()
      '';
    }
  ];
}
