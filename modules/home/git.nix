{ lib, ... }:

{
  programs.difftastic = {
    enable = lib.mkDefault true;
    git = {
      enable = lib.mkDefault true;
      diffToolMode = lib.mkDefault true;
    };
  };

  programs.git = {
    enable = lib.mkDefault true;
    settings.log.date = lib.mkDefault "iso";
  };
}
