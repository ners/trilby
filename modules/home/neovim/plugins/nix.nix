{ pkgs, ... }:

{
  home.packages = with pkgs.unstable; [ nixd nixpkgs-fmt ];

  programs.neovim.extraLuaConfig = /*lua*/ ''
    require('lspconfig').nixd.setup({})
  '';

  xdg.configFile."nvim/ftplugin/nix.lua".text = /*lua*/ ''
    vim.bo.expandtab = true
    vim.bo.shiftwidth = 2
    vim.bo.softtabstop = 2
    vim.bo.tabstop = 2
  '';
}
