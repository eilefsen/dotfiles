source ${ZDOTDIR:-~}/.antidote/antidote.zsh
zstyle ':antidote:bundle' use-friendly-names 'yes'
antidote load

# use prompt
if [ -f $ZDOTDIR/.zshprompt ]; then
    source $ZDOTDIR/.zshprompt
fi

# History
setopt histignorealldups sharehistory
setopt HIST_SAVE_NO_DUPS
HISTSIZE=1000
SAVEHIST=1000

# Vi mode
bindkey -v
ZVM_LINE_INIT_MODE=$ZVM_MODE_INSERT

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        source "$BASE16_SHELL/profile_helper.sh"

base16_helios

# use vi mode cursor
if [ -f $ZDOTDIR/.vimode.zsh ]; then
	source $ZDOTDIR/.vimode.zsh
fi
# use osc7 escape sequence
if [ -f $ZDOTDIR/osc7.zsh ]; then
	source $ZDOTDIR/osc7.zsh
fi
# use aliases
if [ -f $ZDOTDIR/.zshalias ]; then
	source $ZDOTDIR/.zshalias
fi
# use completion
if [ -f $ZDOTDIR/.zshcompletion ]; then
	source $ZDOTDIR/.zshcompletion
fi

# Set the strategy used by autosuggest plugin
ZSH_AUTOSUGGEST_STRATEGY=(completion)
