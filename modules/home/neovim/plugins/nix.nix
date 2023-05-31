{ pkgs, ... }:

{
  home.packages = with pkgs; [ nil nixpkgs-fmt ];

  programs.neovim.extraLuaConfig = ''
    require('lspconfig').nil_ls.setup({})
  '';

  xdg.configFile."nvim/ftplugin/nix.lua".text = ''
    vim.bo.expandtab = true
    vim.bo.shiftwidth = 2
    vim.bo.softtabstop = 2
    vim.bo.tabstop = 2
  '';
}
