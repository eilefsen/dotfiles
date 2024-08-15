# Ensure compinit doesnt needlessly slow down startup time
skip_global_compinit=1

typeset -U PATH path
# XDG Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

### Put various dotfiles in XDG directories
# ZSH
export HISTFILE="$XDG_STATE_HOME"/zsh/zhistory
export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh"
export ADOTDIR="$ZDOTDIR/plugins"
export SHELL_COMPLETIONS_DIR="$ZDOTDIR/completions"
