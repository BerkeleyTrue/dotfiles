# Path to your oh-my-zsh installation.
export ZSH=/home/berkeleytrue/.oh-my-zsh
export DEFAULT_USER=`whoami`
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
  zsh-autosuggestions
)
# set key timeout to 10ms
KEYTIMEOUT=1
ZSH_TMUX_AUTOSTART=true
ZSH_TMUX_AUTOCONNECT=true
ZSH_TMUX_AUTOQUIT=true

# add bound keys
if (( $+commands[xbindkeys] )); then
  xbindkeys
fi

source $ZSH/oh-my-zsh.sh

[[ -s "$HOME/.config/shell/index.sh"  ]] && source "$HOME/.config/shell/index.sh"
