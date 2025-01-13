if ! (( $+functions[zsh-defer] )); then
  fpath+=( $HOME/Library/Caches/antidote/romkatv/zsh-defer )
  source $HOME/Library/Caches/antidote/romkatv/zsh-defer/zsh-defer.plugin.zsh
fi
fpath+=( $HOME/Library/Caches/antidote/joshskidmore/zsh-fzf-history-search )
zsh-defer source $HOME/Library/Caches/antidote/joshskidmore/zsh-fzf-history-search/zsh-fzf-history-search.plugin.zsh
fpath+=( $HOME/Library/Caches/antidote/zsh-users/zsh-autosuggestions )
zsh-defer source $HOME/Library/Caches/antidote/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
fpath+=( $HOME/Library/Caches/antidote/zdharma-continuum/fast-syntax-highlighting )
zsh-defer source $HOME/Library/Caches/antidote/zdharma-continuum/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
fpath+=( $HOME/Library/Caches/antidote/olets/zsh-window-title )
zsh-defer source $HOME/Library/Caches/antidote/olets/zsh-window-title/zsh-window-title.plugin.zsh
