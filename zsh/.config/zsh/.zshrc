# path (macos fucks with path order so it goes in zshrc instead of zshenv)
source ${ZDOTDIR:-~}/path.zsh
source $ZDOTDIR/env.zsh
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

source $ZDOTDIR/zshprompt.zsh
source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/completion.zsh
source $ZDOTDIR/osc7.zsh
source $ZDOTDIR/functions.zsh

# Secrets
if [ -f $ZDOTDIR/secrets.zsh ]; then
	source $ZDOTDIR/secrets.zsh
fi
# local aliases (gitignored)
if [ -f $ZDOTDIR/localaliases.zsh ]; then
	source $ZDOTDIR/localaliases.zsh
fi
# zoxide
eval "$(zoxide init zsh)"
# fzf
source <(fzf --zsh)
