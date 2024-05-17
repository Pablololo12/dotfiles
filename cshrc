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
set magenta="%{\033[1;35m%}"
set yellow="%{\033[1;33m%}"
set endC="%{\033[0m%}"
setenv GIT_BRANCH_CMD "sh -c 'git branch --no-color 2> /dev/null' | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'"
setenv hostnameS "sh -c 'hostname | cut -c-2'"
alias precmd 'set prompt = "${cyan}`$hostnameS`:${green}%c${magenta}`$GIT_BRANCH_CMD`${endC} "'

source ~/.cshrc_secrets
alias emailme /Users/pabher02/utilities/emailme/emailme.py
setenv PATH /opt/homebrew/opt/flex/bin:$PATH
setenv PATH ~/.cargo/bin:$PATH
setenv ORGTODO ~/Documents/Notes
