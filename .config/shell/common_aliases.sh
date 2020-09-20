#!/bin/bash

# Bash helper alias
LSARG='aFl'
LISTC=$(command -v exa > /dev/null 2>&1 && echo 'exa' || echo 'ls --color=tty');
LISTC_N_ARGS=$(command -v exa > /dev/null 2>&1 && echo 'exa -ag'$LSARG || echo 'ls -'$LSARG);

alias ..='cd ..'
alias ...='cd ../.. && pwd'
alias ls=$LISTC
alias ll=$LISTC_N_ARGS
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

# add alias for MR on command
function mr-init() {
  export NODE_PATH=~/dvlpmnt/node/mr/actual/mr_modules
  export NODE_ENV=local
  /usr/bin/docker start mr-mongo mr-mysql-sanitized mr_redis_1 > /dev/null
}

function mr-th() {
  mr-init
  npm run dev-tophat
}

function mr-rv() {
  mr-init
  npm run dev-raven
}

function mr-ws() {
  mr-init
  npm run dev-website
}

function mr-rv-tests() {
  mr-init
  cd ~/dvlpmnt/node/mr/actual/raven
  npm run test
}

function mr-rv-stories() {
  mr-init
  cd ~/dvlpmnt/node/mr/actual/raven
  npm run stories
}
