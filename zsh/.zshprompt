setopt PROMPT_SUBST
# git
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' %b '
# virtualenv
function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo '%F{blue}󰌠 %f%B%F{yellow}['`basename $VIRTUAL_ENV`']%f%b '
}
# python venv
RPROMPT='$(virtualenv_info)'
# time
RPROMPT+="%B%F{blue}[%T]%f%b"
# path 
PROMPT="%B%F{yellow}%~%f%b "
# Git
PROMPT+='%B%F{green}${vcs_info_msg_0_}%f%b'
# end of prompt
PROMPT+="%B$%b "
