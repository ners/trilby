  { config, ... }:

{
  programs.tmux = {
    enable = true;
    mouse = true;
    terminal = "tmux-256color";
    extraConfig = with config.colorScheme.palette; ''
      # COLOUR (base16)

      # default statusbar colours
      set -g status-style "fg=#${base04},bg=#${base01}"

      # use true-colour (24-bit) instead of 256-colour
      set -ga terminal-overrides ",xterm-256color:Tc"

      # default window title colours
      setw -g window-status-style "fg=#${base04},bg=default"

      # active window title colours
      setw -g window-status-current-style "fg=#${base0A},bg=default"

      # pane border
      set -g pane-border-style "fg=#${base01}"
      set -g pane-active-border-style "fg=#${base02}"

      # message text
      set -g message-style "fg=#${base05},bg=#${base01}"

      # pane number display
      set -g display-panes-active-colour "#${base0B}"
      set -g display-panes-colour "#${base0A}"

      # clock
      setw -g clock-mode-colour "#${base0B}"

      # copy mode highligh
      setw -g mode-style "fg=#${base04},bg=#${base02}"

      # bell
      setw -g window-status-bell-style "fg=#${base01},bg=#${base08}"
    '';
  };
}
