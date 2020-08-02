#!/bin/bash

# Bash helper alias
LSARG='aFl'
LISTC=$(command -v exa > /dev/null 2>&1 && echo 'exa -ag'$LSARG || echo 'ls -'$LSARG);

alias ..='cd ..'
alias ...='cd ../.. && pwd'
alias ll=$LISTC
alias v='nvim'
alias vi='nvim'
alias vim='nvim'
## Use vimrc with sudo vim
alias suvim='sudo -E nvim'
alias tmux="TERM=screen-256color-bce tmux -2"
## Will make parent directories if they don't exist
## Also verbose
alias mkdir="mkdir -pv"
alias whatismyip="curl https://ipecho.net/plain; echo"
## Make file executable
alias chmodx='chmod 755'
## Make file read/write-able
alias chmodrw='chmod 644'
## print bash function declaration in console
## Used to make sure profile changes are reflecting
alias echodec='declare -f'

#node aliases
alias :q='exit'

alias npmig='sudo npm install -g'
alias npmug='sudo npm uninstall -g'

alias npmid='npm install --save-dev'
alias npmud='npm uninstall --save-dev'

alias npmis='npm install --save'
alias npmus='npm uninstall --save'
