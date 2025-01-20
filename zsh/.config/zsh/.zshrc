# path (macos fucks with path order so it goes in zshrc instead of zshenv)
source ${ZDOTDIR:-~}/path.zsh
source $ZDOTDIR/env.zsh

# simple plugin management (unplugged)
ZPLUGINDIR=${XDG_DATA_HOME}/zsh/plugins

##? Clone a plugin, identify its init file, source it, and add it to your fpath.
function plugin-load {
  local repo plugdir initfile initfiles=()
  : ${ZPLUGINDIR:=${ZDOTDIR:-~/.config/zsh}/plugins}
  for repo in $@; do
    plugdir=$ZPLUGINDIR/${repo:t}
    initfile=$plugdir/${repo:t}.plugin.zsh
    if [[ ! -d $plugdir ]]; then
      echo "Cloning $repo..."
      git clone -q --depth 1 --recursive --shallow-submodules \
        https://github.com/$repo $plugdir
    fi
    if [[ ! -e $initfile ]]; then
      initfiles=($plugdir/*.{plugin.zsh,zsh-theme,zsh,sh}(N))
      (( $#initfiles )) || { echo >&2 "No init file '$repo'." && continue }
      ln -sf $initfiles[1] $initfile
    fi
    fpath+=$plugdir
    (( $+functions[zsh-defer] )) && zsh-defer . $initfile || . $initfile
  done
}
repos=(
	romkatv/zsh-defer # Defer everything below this line. comment or delete this line to disable deferred loading
	zdharma-continuum/fast-syntax-highlighting
	olets/zsh-window-title
	zsh-users/zsh-autosuggestions
	joshskidmore/zsh-fzf-history-search
)
plugin-load $repos

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
