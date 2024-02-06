{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = lsp_lines-nvim;
      type = "lua";
      config = /*lua*/ ''
        -- Disable virtual_text since it's redundant due to lsp_lines.
        vim.diagnostic.config({
          virtual_text = false,
        })

        require("lsp_lines").setup()
      '';
    }
  ];
}
