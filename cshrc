
alias ls ls -G
alias ll ls -l

set history=500
set savehist=500 merge
set histdup=erase
bindkey "^P" history-search-backward
bindkey "^n" history-search-forward

set autolist=ambiguous

set green="%{\033[0;32m%}"
set cyan="%{\033[0;36m%}"
set end="%{\033[0m%}"
set prompt = "${cyan}%m:${green}%c${end} "
unset green cyan end

set path = (/usr/bin $path)
set path = ($path /Users/pablo/bin)
