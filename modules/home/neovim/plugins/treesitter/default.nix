{ pkgs, ... }:

{
  programs.neovim.plugins = with pkgs.unstable.vimPlugins; [
    {
      plugin = nvim-treesitter.withAllGrammars;
      type = "lua";
      config = builtins.readFile ./config.lua;
    }
    nvim-ts-rainbow2
    nvim-treesitter-textobjects
    nvim-treesitter-refactor
  ];
}
