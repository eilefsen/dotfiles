# path (macos fucks with path order so it goes in zshrc instead of zshenv)
source ${ZDOTDIR:-~}/path.zsh
source $ZDOTDIR/env.zsh

# plugins
source "$ZDOTDIR/plugin-loader.zsh"
plugin_repos=(
	mroth/evalcache
	romkatv/zsh-defer # everything below is deferred
	zdharma-continuum/fast-syntax-highlighting
	zsh-users/zsh-autosuggestions
	olets/zsh-window-title
	joshskidmore/zsh-fzf-history-search
)
separately_loaded_plugin_repos=(
	romkatv/zsh-bench
)
export PATH="$ZPLUGINDIR/zsh-bench:$PATH"

local plugins=($(get-plugin-name-from-repo $plugin_repos));
alias source-zsh-plugins="plugin-source $plugins";
alias install-zsh-plugins="plugin-clone $plugin_repos $separately_loaded_plugin_repos && source-zsh-plugins"
install-zsh-plugins
source-zsh-plugins

# fzf, load this before fzf-history-search plugin
_evalcache fzf --zsh # not deferred, we want this to happen before loading the deferred plugin
# zoxide
zsh-defer _evalcache zoxide init zsh

git-branch() {
  local ref=$(git symbolic-ref --short HEAD 2> /dev/null)
  if [ -n "${ref}" ]; then
   if [ -n "$(git diff --quiet HEAD)" ]; then
	   local gitstatuscolor='%F{red}'
   else
	   local gitstatuscolor='%F{green}'
   fi
   echo "${gitstatuscolor} (${ref}) "
  else
   echo ""
  fi
}
setopt PROMPT_SUBST
PROMPT='%B%F{yellow}%~%f%b $(git-branch) %f%b%B$%b '

# History
setopt histignorealldups sharehistory
setopt HIST_SAVE_NO_DUPS
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=${XDG_STATE_HOME}/.zsh_history

# Set the strategy used by autosuggest plugin
ZSH_AUTOSUGGEST_STRATEGY=(completion)

bindkey -e

source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/completion.zsh"
# source "$ZDOTDIR/osc7.zsh"
source "$ZDOTDIR/functions.zsh"

# Secrets
if [ -f "$ZDOTDIR/secrets.zsh" ]; then
	source "$ZDOTDIR/secrets.zsh"
fi
# local aliases (gitignored)
if [ -f "$ZDOTDIR/localaliases.zsh" ]; then
	source "$ZDOTDIR/localaliases.zsh"
fi

