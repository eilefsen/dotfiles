# Antidote plugin manager
source ${ZDOTDIR:-~}/.antidote/antidote.zsh
zstyle ':antidote:bundle' use-friendly-names 'yes'
antidote load

$ZDOTDIR/base16_theme.sh

# History
setopt histignorealldups sharehistory
setopt HIST_SAVE_NO_DUPS
HISTSIZE=1000
SAVEHIST=1000

# Set the strategy used by autosuggest plugin
ZSH_AUTOSUGGEST_STRATEGY=(completion)

# Prompt
if [ -f $ZDOTDIR/.zshprompt ]; then
    source $ZDOTDIR/.zshprompt
fi

# Aliases
if [ -f $ZDOTDIR/.zshalias ]; then
	source $ZDOTDIR/.zshalias
fi

# Completion
if [ -f $ZDOTDIR/.zshcompletion ]; then
	source $ZDOTDIR/.zshcompletion
fi

# Vi mode
if [ -f $ZDOTDIR/vimode.zsh ]; then
	source $ZDOTDIR/vimode.zsh
fi

# Osc7 escape sequence
if [ -f $ZDOTDIR/osc7.zsh ]; then
	source $ZDOTDIR/osc7.zsh
fi
