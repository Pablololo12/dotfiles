set-option -ga terminal-overrides ",xterm-256color:Tc"
set-option -g default-terminal "screen-256color"

# To make it like screen
bind-key C-b last-window

# Fix insert mode lag in vim
set -sg escape-time 0
set-window-option -g mode-keys vi

# Start windows and panes at 1.
set -g base-index 1
set -g pane-base-index 1
set -g renumber-windows on
