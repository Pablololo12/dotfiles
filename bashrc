###########
# Aliases #
###########

set -o vi
alias ls="ls --color=auto"
alias ll="ls -l"
alias topp="top -u $user"

export COLORTERM=truecolor

###########
# History #
###########

HISTSIZE=5000
HISTFILESIZE=10000
shopt -s histappend
bind '"\C-p": history-search-backward'
bind '"\C-n": history-search-forward'

export GIT_PAGER=less
export LESS="-RF"

##########
# Prompt #
##########

git_branch_cmd() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

hostnameS() {
    hostname | cut -c-2
}

_GREEN='\[\033[0;32m\]'
_BLUE='\[\033[0;34m\]'
_CYAN='\[\033[0;36m\]'
_RED='\[\033[0;31m\]'
_YELLOW='\[\033[0;33m\]'
_MAGENTA='\[\033[0;35m\]'
_RESET='\[\033[0m\]'
_BOLD='\[\033[1m\]'

PS1="${_CYAN}\$(hostnameS)${_RED}:${_GREEN}\W${_MAGENTA}\$(git_branch_cmd)${_RESET} "

t() {
    env TERM=screen-256color tmux -u a
    if [ $? -ne 0 ]; then
      env TERM=screen-256color tmux -u
    fi
}

export TERM=xterm-256color

# For work related configs

if [ -f ~/.bashrc_secrets ]; then
  source ~/.bashrc_secrets
fi
