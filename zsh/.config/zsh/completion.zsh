zmodload zsh/complist

fpath=($ZDOTDIR/completions $fpath)


ZSH_CACHE_PATH="$XDG_CACHE_HOME/zsh/"
ZSH_COMPDUMP="$ZSH_CACHE_PATH/.zcompdump"
autoload -Uz compinit

if [ ! -f $ZSH_COMPDUMP ] || [[ "$(find $ZSH_COMPDUMP -mmin +1440)" ]] {
	# Regenerate completions because the dump file hasn't been modified within the last 24 hours
    compinit -d "$ZSH_COMPDUMP"
} else {
	# simply read the cache
	compinit -C -d "$ZSH_COMPDUMP"
}

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$ZSH_CACHE_PATH/comp/"

# keybinds
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'w' vi-forward-blank-word
bindkey -M menuselect 'b' vi-backward-blank-word
# setopt
setopt globdots
setopt ALWAYS_TO_END
setopt AUTO_MENU
setopt AUTO_PARAM_SLASH
setopt LIST_ROWS_FIRST

# some basic setup
zstyle ':completion:*' menu select hidden
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:*:*:descriptions' format '%F{14}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{11}!- %d (errors: %e) -!%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red} -- no matches found --%f'
# completion rules
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*:*:-command-' group-order alias builtins functions commands
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' complete-options true
zstyle ':completion:*' file-sort
