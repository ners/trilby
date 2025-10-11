{
  trilby,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    fzf
    zsh-completions
  ];

  programs.zsh = {
    enable = true;
    autocd = true;

    autosuggestion.enable = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    historySubstringSearch.enable = true;

    history = {
      expireDuplicatesFirst = true;
      ignoreDups = true;
      ignoreAllDups = true;
      ignoreSpace = true;
      save = 100000;
      size = 100000;
      share = true;
    };

    plugins = [
      {
        name = "fzf-tab";
        src = "${pkgs.zsh-fzf-tab}/share/fzf-tab";
      }
    ];

    shellAliases = {
      ls = "ls --color=auto";
    };
  }
  // (
    if lib.versionAtLeast trilby.release "25.05" then
      { initContent = builtins.readFile ./init.sh; }
    else
      { initExtra = builtins.readFile ./init.sh; }
  );
}
