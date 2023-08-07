{ pkgs, ... }:

{
  home.packages = with pkgs; [
    stylua
    shfmt
    nixpkgs-fmt
    python3Packages.black
  ];
  programs.neovim.plugins = with pkgs.vimPlugins; [ null-ls-nvim ];
}
