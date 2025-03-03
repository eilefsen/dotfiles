if [ -f $ZDOTDIR/workaliases.zsh ]; then
    source $ZDOTDIR/workaliases.zsh
fi

# ls (lsd)
    alias ls='lsd -1'
    alias ll="ls -lAh"
    alias l="ll"
    # show total size of directories
    alias lt="ll --total-size"
    # date
    alias lda="ls -lAh --blocks permission,date,user,group,size,name"
    # git
    alias lg="ls -lAh --blocks git,name"
    alias lgt="lg --tree"
    # tree
    alias tree="ls -A --tree"
# color support
if [ -x /usr/bin/dircolors ]; then
	test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
	alias dir='dir --color=auto'
	alias vdir='vdir --color=auto'
	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
fi
# set different hsts file location for wget
alias -g wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

# Python
# alias python='python3'
alias vactivate='source ./venv/bin/activate'

# Rsync
alias websync='rsync -azv --cvs-exclude'

# Git
alias gs='git status'
alias ga='git add'
alias gc='git commit'

ssh () { command ssh "$@"; exec zsh; }
# kittens
alias icat='kitty +kitten icat'
alias kssh='kitten ssh'
# esp-idf
alias get_idf='. $HOME/Code/esp/esp-idf/export.sh; export CLANGD_BIN=$HOME/.espressif/tools/esp-clang/16.0.1-fe4f10a809/esp-clang/bin/clangd'

#docker
alias dcu='docker compose up -d'
alias dcd='docker compose down'
alias dcr='docker compose run --rm'
alias dcrs='docker compose restart'
alias dcdu='docker compose down && docker compose up -d'

#cmake
alias cmk='cmake . -B build'

# Void linux
alias xi='xbps-install'
alias xq='xbps-query'
alias sudo='sudo '
