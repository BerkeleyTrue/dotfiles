#!/bin/sh
# Bash helper alias
LSARG='aFl'
LISTC=$(command -v exa >/dev/null 2>&1 && echo 'exa' || echo 'ls --color=tty')
LISTC_N_ARGS=$(command -v exa >/dev/null 2>&1 && echo 'exa -ag'$LSARG || echo 'ls -'$LSARG)

alias ..='cd ..'
alias ...='cd ../.. && pwd'
alias ls=$LISTC
alias ll=$LISTC_N_ARGS
alias man=$(command -v batman >/dev/null 2>&1 && echo 'batman' || echo 'man')
alias cat=$(command -v bat >/dev/null 2>&1 && echo 'bat' || echo 'cat')

alias v='nvim'
alias vi='nvim'
alias vim='nvim'
# alias nvim='nvim'
## Use vimrc with sudo vim
alias suvim='sudo -E nvim'
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

# taskwarrior
alias tadd='t add'

tcon() {
 if [[ $# -eq 0 ]]; then
   task context show;
   return 0;
 fi

 shift
 eval "task context $*"
}
# tmod 14 proj:foo
tmod() {
  local num=$1
  shift
  eval "task $num mod $*"
}
