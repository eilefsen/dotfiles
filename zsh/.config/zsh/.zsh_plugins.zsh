if ! (( $+functions[zsh-defer] )); then
  fpath+=( /Users/emma/Library/Caches/antidote/romkatv/zsh-defer )
  source /Users/emma/Library/Caches/antidote/romkatv/zsh-defer/zsh-defer.plugin.zsh
fi
fpath+=( /Users/emma/Library/Caches/antidote/zsh-users/zsh-autosuggestions )
zsh-defer source /Users/emma/Library/Caches/antidote/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
fpath+=( /Users/emma/Library/Caches/antidote/zdharma-continuum/fast-syntax-highlighting )
zsh-defer source /Users/emma/Library/Caches/antidote/zdharma-continuum/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
fpath+=( /Users/emma/Library/Caches/antidote/olets/zsh-window-title )
zsh-defer source /Users/emma/Library/Caches/antidote/olets/zsh-window-title/zsh-window-title.plugin.zsh
