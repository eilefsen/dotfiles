prompt_emma_setup() {
    PROMPT=' %F{yellow}%~%f %B%F{red}${vcs_info_msg_0_}%f%b$ '
    RPS1='%B%F{blue}[%T]%f%b'
    if [[ "$VIRTUAL_ENV" != "" ]]; then
        RPS1='[%B%F{red}$(virtualenv_info)]%f%b $RPS1' 
    fi
}
