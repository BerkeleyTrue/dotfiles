#!/bin/bash
# Bash helper alias

alias c='clear'
alias _='sudo'

if [[ -n $(command -v eza) ]]; then
	alias ls='eza'
	alias ll='eza -aglF'
else
  echo "eza not found"
	alias ls='ls --color=tty'
	alias ll='ls -aFl'
fi

alias ..='cd ..'
alias ...='cd ../.. && pwd'

if [[ -n $(command -v batman) ]]; then
	alias cat='batman --theme Dracula'
else
	alias cat='cat'
fi

if [[ -n $(command -v bat) ]]; then
	alias cat='bat --theme Dracula'
else
	alias cat='cat'
fi

alias v='nvim'
alias vi='nvim'
alias vim='nvim'
# alias nvim='nvim'
## Use vimrc with sudo vim
alias suvim='sudo -E nvim'
# open vim without autosession
v-() {
 # pass all args to nvim
 echo "" | nvim $@
}
## Will make parent directories if they don't exist
## Also verbose
alias mkdir="mkdir -pv"
alias getexternalip="curl http://icanhazip.com"
## Make file executable
alias chmodx='chmod 755'
## Make file read/write-able
alias chmodrw='chmod 644'
## print bash function declaration in console
## Used to make sure profile changes are reflecting
alias echodec='declare -f'

alias :q='exit'
alias :wq='exit'

# ufw
alias ufw="sudo ufw"
