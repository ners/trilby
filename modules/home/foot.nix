{ trilby, config, lib, ... }:

{
  programs.foot = {
    enable = lib.mkDefault (trilby.edition == "workstation");
    settings = {
      main = {
        term = "xterm-256color";
        font = "Iosevka Nerd Font:size=11";
        pad = "10x5 center";
      };
      mouse = { hide-when-typing = "yes"; };
      colors = with config.colorScheme.palette; {
        alpha = 0.85;
        background = base00;
        foreground = base05;
        regular0 = base00; # black
        regular1 = base08; # red
        regular2 = base0B; # green
        regular3 = base0A; # yellow
        regular4 = base0D; # blue
        regular5 = base0E; # magenta
        regular6 = base0C; # cyan
        regular7 = base05; # white
        bright0 = base03; # bright black
        bright1 = base08; # bright red
        bright2 = base0B; # bright green
        bright3 = base0A; # bright yellow
        bright4 = base0D; # bright blue
        bright5 = base0E; # bright magenta
        bright6 = base0C; # bright cyan
        bright7 = base07; # bright white
      };
    };
  };
}
