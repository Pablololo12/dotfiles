###########
# Aliases #
###########

bindkey -v
alias ls ls --color=auto
alias ll ls -l
alias topp top -u $user

###########
# History #
###########

set history=500
set savehist=500 merge
set histdup=erase
bindkey "^P" history-search-backward
bindkey "^n" history-search-forward
set autolist=ambiguous

##########
# Prompt #
##########

set green="%{\033[0;32m%}"
set cyan="%{\033[0;36m%}"
set end="%{\033[0m%}"
set prompt = "${cyan}%m:${green}%c${end} "
unset green cyan end

set path = ($path $home/bin)
