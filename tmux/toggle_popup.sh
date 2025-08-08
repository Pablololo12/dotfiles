#!/bin/bash

if tmux list-sessions | grep -q 'scratch.*attached'; then
    tmux detach-client -s scratch
else
    tmux display-popup -E "tmux new-session -A -s scratch"
fi

