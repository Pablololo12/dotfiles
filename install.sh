#!/bin/bash

link() {
    if ! [ -e ~/.$1 ]; then
        ln -s `pwd`/$1 ~/.$1
    fi
}

link_folder() {
    if ! [ -d ~/.$1 ]; then
        ln -s `pwd`/$1 ~/.$1
    fi
}

link_config() {
    mkdir -p ~/.config
    if ! [ -d ~/.config/$1 ]; then
        ln -s `pwd`/$1 ~/.config/$1
    fi
}

link "bashrc"
link "screenrc"
link_folder "emacs.d"
link_folder "lisp-utils"
link_config "tmux"
link_config "nvim"
