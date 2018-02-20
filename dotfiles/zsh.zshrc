# Path to your oh-my-zsh installation.
export ZSH=/home/berkeleytrue/.oh-my-zsh
ZSH_THEME="agnoster"
# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"
# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"
plugins=(
  archlinux
  npm
  tig
  tmux
  vi-mode
  wd
)

source $ZSH/oh-my-zsh.sh

# [[ -s "$HOME/.config/shell/index.sh"  ]] && source "$HOME/.config/shell/index.sh" || echo "no shell folder found"
source "$HOME/.config/shell/index.sh"
