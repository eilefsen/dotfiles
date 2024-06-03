fpath+=( /home/emma/.cache/antidote/jeffreytse/zsh-vi-mode )
source /home/emma/.cache/antidote/jeffreytse/zsh-vi-mode/zsh-vi-mode.plugin.zsh
if ! (( $+functions[zsh-defer] )); then
  fpath+=( /home/emma/.cache/antidote/romkatv/zsh-defer )
  source /home/emma/.cache/antidote/romkatv/zsh-defer/zsh-defer.plugin.zsh
fi
fpath+=( /home/emma/.cache/antidote/zsh-users/zsh-autosuggestions )
zsh-defer source /home/emma/.cache/antidote/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
fpath+=( /home/emma/.cache/antidote/zdharma-continuum/fast-syntax-highlighting )
zsh-defer source /home/emma/.cache/antidote/zdharma-continuum/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
fpath+=( /home/emma/.cache/antidote/olets/zsh-window-title )
zsh-defer source /home/emma/.cache/antidote/olets/zsh-window-title/zsh-window-title.plugin.zsh
