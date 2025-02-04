#!/bin/tcsh
mkdir -p ~/.config

alias link_if_not_exist 'if (! -l ~/.\!:2 ) ln -s `pwd`/\!:1 ~/.\!:2'

link_if_not_exist vimrc vimrc
link_if_not_exist cshrc cshrc
link_if_not_exist screenrc screenrc
link_if_not_exist tmux config/tmux.conf
link_if_not_exist nvim config/nvim
link_if_not_exist emacs.d emacs.d
link_if_not_exist wezterm config/wezterm
link_if_not_exist zellij config/zellij
