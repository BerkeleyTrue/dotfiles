# Path to your oh-my-zsh installation.
export ZSH=/home/berkeleytrue/.oh-my-zsh
export DEFAULT_USER=`whoami`
TERMINAL="hyper"
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
# Automatically start tmux on zsh source
ZSH_TMUX_AUTOSTART=true
# don't quit the terminal when detaching from tmux
ZSH_TMUX_AUTOQUIT=false
# automatically connect to tmux session if there is one runnning
ZSH_TMUX_AUTOCONNECT=true
# add bound keys
if (( $+commands[xbindkeys] )); then
  xbindkeys
fi

###
# zsh use primary clipboard for vi-keys
####
function x11-clip-wrap-widgets() {
  local copy_or_paste=$1
  shift
  for widget in $@; do
    if [[ $copy_or_paste == "copy" ]]; then
      eval "
      function _x11-clip-wrapped-$widget() {
        zle .$widget
        xclip -in -selection clipboard <<<\$CUTBUFFER
      }
      "
    else
      eval "
      function _x11-clip-wrapped-$widget() {
        CUTBUFFER=\$(xclip -out -selection clipboard)
        zle .$widget
      }
      "
    fi
    zle -N $widget _x11-clip-wrapped-$widget
  done
}
local copy_widgets=(
  vi-yank vi-yank-eol vi-delete vi-backward-kill-word vi-change-whole-line
)
local paste_widgets=(
  vi-put-{before,after}
)
x11-clip-wrap-widgets copy $copy_widgets
x11-clip-wrap-widgets paste  $paste_widgets
### end-clipboard paste ###
source $ZSH/oh-my-zsh.sh


### change cursor shape in xterm ###
# must come after oh-my-zsh vim plugin
BLOCK="\e[1 q"
BEAM="\e[5 q"
function zle-keymap-select zle-line-init {
  if [ $KEYMAP = vicmd ]; then
    # the command mode for vi
    print -n "$BLOCK"
  else
    # the insert mode for vi
    print -n "$BEAM"
  fi
  zle reset-prompt
  zle -R
}

function zle-line-finish {
  print -n -- "$BLOCK"  # block cursor
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select
### end cursor mod ###

zle -N zle-keymap-select
### end cursor mod ###
[[ -s "$HOME/.config/shell/index.sh"  ]] && source "$HOME/.config/shell/index.sh"
