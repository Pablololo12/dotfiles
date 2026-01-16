
###########
# Aliases #
###########

alias ls='ls --color=auto -hv'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
alias ll='ls -l'
alias la='ls -lA'
alias topp="top -u $user"

###########
# History #
###########

HISTSIZE=5000
HISTFILESIZE=10000
setopt inc_append_history
bindkey "\C-p" history-search-backward
bindkey "\C-n" history-search-forward

################
# Autocomplete #
################

autoload -U compinit && compinit

##########
# Prompt #
##########

autoload -Uz vcs_info
setopt PROMPT_SUBST
precmd() { vcs_info }
PROMPT='%F{cyan}${SSH_CONNECTION:+${HOST[1,2]}:}%f'
PROMPT+='%F{green}%1~%f'
PROMPT+=' %(?.%F{magenta}.%F{red})λ%f '

RPROMPT='%F{magenta}${vcs_info_msg_0_}%f'

zstyle ':vcs_info:git:*' formats ' (%b)'
