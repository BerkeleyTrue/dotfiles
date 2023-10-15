#!/bin/sh
# Make neovim the default editor
export EDITOR=nvim
export VISUAL=$EDITOR

# Add lang settings for fonts to work properly
export LANG=en_US.UTF-8
export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_ALL=
export SSH_KEY_PATH="~/.ssh/rsa_id"
