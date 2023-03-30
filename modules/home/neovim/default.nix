{ pkgs, lib, ... }@args:

{
  imports = builtins.attrValues (lib.findModules ./plugins);

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
  };
}
