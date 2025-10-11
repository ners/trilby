{
  trilby,
  lib,
  pkgs,
  ...
}:

{
  programs = {
    zsh.enable = true;
  };

  environment.shells = [ pkgs.zsh ];
}
// lib.optionalAttrs (trilby.hostSystem.kernel.name == "linux") {
  users.defaultUserShell = pkgs.zsh;
}
