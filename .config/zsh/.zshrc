# clear right prompt on execute
setopt TRANSIENT_RPROMPT

export DEFAULT_USER=`whoami`
export FZF_DEFAULT_COMMAND='fd .'
export WD_CONFIG="$XDG_CONFIG_HOME/warpdrive/warprc"
export TASKDATA="$XDG_CONFIG_HOME/task"
export TASKRC="$TASKDATA/taskrc"

ZSH_CACHE_DIR="$HOME/.cache/zsh"
ZSH="$XDG_CONFIG_HOME/zsh"
HISTFILE="$XDG_DATA_HOME/zsh/.zsh_history"

# Antigen configs
ADOTDIR="$XDG_CONFIG_HOME/antigen"
ANTIGEN_LOG="$HOME/.cache/antigen/antigen.log"
OSNAME=$(uname)
# uncomment to debug antigen
# export ANTIGEN_LOG=$HOME/.antigen/antigen.log
TERMINAL="alacritty"
ZSH_THEME="agnoster"
# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"
# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS=true

fpath+="$ZSH/.zfunc"

# set key timeout to 10ms
KEYTIMEOUT=1
# Automatically start tmux on zsh source
ZSH_TMUX_AUTOSTART=false
# don't quit the terminal when detaching from tmux
ZSH_TMUX_AUTOQUIT=false
# automatically connect to tmux session if there is one runnning
ZSH_TMUX_AUTOCONNECT=false
# add bound keys
if (( $+commands[xbindkeys] )); then
  xbindkeys
fi

# ensure cache directory exists
if [[ ! -f $ANTIGEN_LOG  ]]; then
  mkdir -p $(dirname $ANTIGEN_LOG) > /dev/null 2>&1;
fi

# source antigen plugin manager
if [[ $OSNAME = 'Darwin' ]]; then
  source /usr/local/share/antigen/antigen.zsh
else
  source /usr/share/zsh/share/antigen.zsh
fi

antigen use oh-my-zsh

antigen bundle djui/alias-tips
antigen bundle lukechilds/zsh-better-npm-completion
antigen bundle tmux
antigen bundle wd
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle bgnotify
antigen bundle docker
antigen bundle terraform
antigen bundle command-not-found
antigen bundle jeffreytse/zsh-vi-mode

antigen theme $ZSH ghanima

# apply antigen plugins
antigen apply


# apply zoxide to zsh
eval "$(zoxide init zsh)"

###
# zsh use primary clipboard for vi-keys
####
function x11-clip-wrap-widgets() {
  local copy_or_paste=$1
  shift
  local copy_command='xclip -in -selection clipboard'
  local paste_command='xclip -out -selection clipboard'

  [[ $OSNAME == 'Darwin' ]] && copy_command='pbcopy'
  [[ $OSNAME == 'Darwin' ]] && paste_command='pbpaste'

  for widget in $@; do
    if [[ $copy_or_paste == "copy" ]]; then
      eval "
      function _x11-clip-wrapped-$widget() {
        zle .$widget
        $copy_command <<<\$CUTBUFFER
      }
      "
    else
      eval "
      function _x11-clip-wrapped-$widget() {
        CUTBUFFER=\$($paste_command)
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

SHORT_PROMPT="%F{cyan}%f "
function zle-line-finish {
  if [[ $PROMPT != $SHORT_PROMPT ]]; then
    PROMPT=$SHORT_PROMPT
    if [[ $RPROMPT != "" ]]; then
      RPROMPT=""
    fi
    zle .reset-prompt
  fi
  print -n -- "$BLOCK"  # block cursor
}

zle -N zle-line-init
zle -N zle-line-finish
zle -N zle-keymap-select
### end cursor mod ###
[[ -s "$XDG_CONFIG_HOME/shell/index.sh"  ]] && source "$XDG_CONFIG_HOME/shell/index.sh"

[[ -s  "$XDG_CONFIG_HOME/broot/launcher/bash/br" ]] && source "$XDG_CONFIG_HOME/broot/launcher/bash/br"

if [[ $OSNAME != 'Darwin' ]]; then
  source /usr/share/fzf/completion.zsh
  source /usr/share/fzf/key-bindings.zsh
fi

autoload -U compinit && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
