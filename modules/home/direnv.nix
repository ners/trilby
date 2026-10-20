{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    config.global.load_dotenv = true;
  };
  home.sessionVariables.DIRENV_LOG_FORMAT = "";
}
