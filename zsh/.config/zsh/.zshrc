# path (macos fucks with path order so it goes in zshrc instead of zshenv)
source ${ZDOTDIR:-~}/path.zsh
# Antidote plugin manager
source ${ZDOTDIR:-~}/.antidote/antidote.zsh
zstyle ':antidote:bundle' use-friendly-names 'yes'
antidote load

autoload -Uz md

# History
setopt histignorealldups sharehistory
setopt HIST_SAVE_NO_DUPS
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Set the strategy used by autosuggest plugin
ZSH_AUTOSUGGEST_STRATEGY=(completion)

bindkey -e

# Prompt
if [ -f $ZDOTDIR/zshprompt.zsh ]; then
    source $ZDOTDIR/zshprompt.zsh
fi

# Aliases
if [ -f $ZDOTDIR/aliases.zsh ]; then
	source $ZDOTDIR/aliases.zsh
fi

# Completion
if [ -f $ZDOTDIR/completion.zsh ]; then
	source $ZDOTDIR/completion.zsh
fi

# Osc7 escape sequence
if [ -f $ZDOTDIR/osc7.zsh ]; then
	source $ZDOTDIR/osc7.zsh
fi

# Secrets
if [ -f $ZDOTDIR/secrets.zsh ]; then
	source $ZDOTDIR/secrets.zsh
fi
# zoxide
eval "$(zoxide init zsh)"
# eval "$(fzf --zsh)"
