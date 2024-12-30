{ ... }:

{
  programs.git = {
    enable = true;
    difftastic.enable = true;
    extraConfig.log.date = "iso";
  };
}
