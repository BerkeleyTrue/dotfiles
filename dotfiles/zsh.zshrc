# Path to your oh-my-zsh installation.
export ZSH=/home/berkeleytrue/.oh-my-zsh
export PATH="$PATH:$(ruby -e 'print Gem.user_dir')/bin"

ZSH_THEME="agnoster"


# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"
# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

plugins=(
  archlinux
  docker
  git
  npm
  tig
  tmux
  vi-mode
  wd
)

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

export SSH_KEY_PATH="~/.ssh/rsa_id"
alias git=hub


# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
