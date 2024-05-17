###########
# Aliases #
###########

set -o vi
alias ls="ls --color=auto"
alias ll="ls -l"
alias topp="top -u $user"

###########
# History #
###########

HISTSIZE=5000
HISTFILESIZE=10000
shopt -s histappend
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'

##########
# Prompt #
##########

git_branch_cmd() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

hostnameS() {
    hostname | cut -c-2
}

_GREEN=$(tput setaf 2)
_BLUE=$(tput setaf 4)
_CYAN=$(tput setaf 6)
_RED=$(tput setaf 1)
_YELLOW=$(tput setaf 3)
_MAGENTA=$(tput setaf 5)
_RESET=$(tput sgr0)
_BOLD=$(tput bold)
PS1='${_CYAN}$(hostnameS)${_RED}:${_GREEN}\W${_MAGENTA}$(git_branch_cmd)${_RESET} '

# For work related configs

if [ -f ~/.bashrc_secrets ]; then
  source ~/.bashrc_secrets
fi
