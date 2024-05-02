{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.vimPlugins; [
    {
      plugin = nvim-treesitter.withAllGrammars;
      type = "lua";
      config = builtins.readFile ./config.lua;
    }
    rainbow-delimiters-nvim
    nvim-treesitter-textobjects
    nvim-treesitter-refactor
  ];
}
