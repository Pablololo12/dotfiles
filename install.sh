#!/bin/bash

link() {
    if ! [ -e ~/.$1 ]; then
        ln -s `pwd`/$1 ~/.$1
    fi
}

link_config() {
    mkdir -p ~/.config
    if ! [ -e ~/.$1 ]; then
        ln -s `pwd`/$1 ~/.config/.$1
    fi
}

link "bashrc"
link "vimrc"
link "cshrc"
link "screenrc"
link "emacs.d"
link_config "tmux"
link_config "nvim"
link_config "wezterm"
link_config "zellij"
