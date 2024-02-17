set -g default-terminal "tmux-256color"
set -ga terminal-overrides ",*256col*:Tc"


# To make it like screen
bind-key C-b last-window
bind r command-prompt "rename-window %%" # rename mapped to r

bind | split-window -h
bind - split-window -v
unbind '"'
unbind %

# don't do anything when a 'bell' rings
set -g visual-activity off
set -g visual-bell off
set -g visual-silence off
setw -g monitor-activity off
set -g bell-action none

# clock mode
setw -g clock-mode-colour colour1

# copy mode
setw -g mode-style 'fg=colour1 bg=colour18 bold'

# Fix insert mode lag in vim
set -sg escape-time 0
set-window-option -g mode-keys vi

# Start windows and panes at 1.
set -g base-index 1
set -g pane-base-index 1
set -g renumber-windows on

set -g history-limit 1000000     # increase history size (from 2,000)
set -g set-clipboard on          # use system clipboard
set -g status-position top       # macOS / darwin style

set -g renumber-windows on       # renumber all windows when any window is closed

# overall panel style
set -g status-justify left
set -g status-style 'fg=colour1'
set -g status-left ''
set -g status-right '%Y-%m-%d %H:%M '
set -g status-right-length 50
set -g status-left-length 10
# pane borders
set -g pane-border-style 'fg=colour1'
set -g pane-active-border-style 'fg=colour3'

setw -g window-status-current-style 'fg=colour0 bg=colour1 bold'
setw -g window-status-current-format ' #I #W #F '

setw -g window-status-style 'fg=colour1 dim'
setw -g window-status-format ' #I #[fg=colour7]#W #[fg=colour1]#F '

setw -g window-status-bell-style 'fg=colour2 bg=colour1 bold'

# messages
set -g message-style 'fg=colour2 bg=colour0 bold'
