{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = catppuccin-nvim;
      type = "lua";
      config = /*lua*/ ''
        require("catppuccin").setup({
            flavour = "mocha",
            integrations = {
                cmp = true,
                gitsigns = true,
                treesitter = true,
                treesitter_context = true,
            },
        })

        vim.cmd.colorscheme "catppuccin"
      '';
    }
  ];
}
