fpath+=($HOME/.nix-profile/share/zsh/site-functions)
# Reload the zsh-completions
autoload -U compinit && compinit

# Pretty colours in less command
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;38;5;74m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[38;5;246m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;146m'

# Functional Home-/End-/Delete-/Insert-keys
bindkey '\e[1~'   beginning-of-line  # Linux console
bindkey '\e[H'    beginning-of-line  # xterm
bindkey '\e[2~'   overwrite-mode     # Linux console, xterm, gnome-terminal
bindkey '\e[3~'   delete-char        # Linux console, xterm, gnome-terminal
bindkey '\e[4~'   end-of-line        # Linux console
bindkey '\e[F'    end-of-line        # xterm
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

bindkey "^[[A" history-substring-search-up
bindkey "^[[B" history-substring-search-down

# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"

# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
