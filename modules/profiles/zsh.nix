{ pkgs, lib, ... }:

lib.mkMerge [
  {
    programs.zsh.enable = true;
    programs.starship.enable = true;
  }
  (lib.optionalAttrs pkgs.parsedSystem.isLinux {
    environment.shells = [ pkgs.zsh ];
    users.defaultUserShell = pkgs.zsh;
  })
]
