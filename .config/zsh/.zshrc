# Zsh configuration
# runs on every new shell

# clear right prompt on execute
setopt TRANSIENT_RPROMPT

OSNAME=$(uname)
export DEFAULT_USER=`whoami`
export FZF_DEFAULT_COMMAND='fd .'
export WD_CONFIG="$XDG_CONFIG_HOME/warpdrive/warprc"
export TASKDATA="$XDG_CONFIG_HOME/task"
export TASKRC="$TASKDATA/taskrc"
export TIMEWARRIORDB="$XDG_CONFIG_HOME/timewarrior"
export GOPATH=$HOME/dvlpmnt/go
export XDG_CURRENT_DESKTOP=Unity

path+=($HOME/.local/bin) # local binaries and scripts
path+=($GOPATH/bin) # go binaries
path+=($HOME/.cargo/bin) # rust binaries
path+=($HOME/.nix-profile/bin) # nix binaries

XDG_CONFIG_HOME="$HOME/.config"
XDG_DATA_HOME="$HOME/.local/share"
ZSH_CACHE_DIR="$HOME/.cache/zsh"
ZSH="$XDG_CONFIG_HOME/zsh"
mkdir -p "$XDG_DATA_HOME/zsh"
HISTFILE="$XDG_DATA_HOME/zsh/.zsh_history"

# Antigen configs
ADOTDIR="$XDG_CONFIG_HOME/antigen"
ANTIGEN_LOG="$HOME/.cache/antigen/antigen.log"
# uncomment to debug antigen
# export ANTIGEN_LOG=$HOME/.antigen/antigen.log
TERMINAL="kitty"
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

# ensure cache directory exists
if [[ ! -f $ANTIGEN_LOG  ]]; then
  mkdir -p $(dirname $ANTIGEN_LOG) > /dev/null 2>&1;
fi

# source antigen plugin manager
if [[ $OSNAME = 'Darwin' ]]; then

  # start nix daemon
  if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
    . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
  fi

  # check if homebrew bin exits and add it to path
  if [ -d '/opt/homebrew/bin' ]; then
    path+=(/opt/homebrew/bin) # homebrew binaries
    command -v /opt/homebrew/bin/brew > /dev/null \
      && eval "$(/opt/homebrew/bin/brew shellenv)" \
      || echo "brew not found"
  fi

  # check if antigen is installed
  if [ -e "$HOME/.nix-profile/share/antigen/antigen.zsh" ]; then
    source $HOME/.nix-profile/share/antigen/antigen.zsh
  else
    echo "antigen not found"
  fi

else
  source /usr/share/zsh/share/antigen.zsh #TODO: move to nix on linux machines as well
fi

antigen use oh-my-zsh

antigen bundle djui/alias-tips
antigen bundle lukechilds/zsh-better-npm-completion
antigen bundle wd
antigen bundle vi-mode
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle bgnotify
antigen bundle docker
antigen bundle terraform
antigen bundle command-not-found
antigen bundle taskwarrior
antigen bundle systemd
antigen bundle ufw

antigen theme $ZSH ghanima

# apply antigen plugins
antigen apply

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

# 
SHORT_PROMPT="%F{cyan}=<<%f%F{blue}%*%f%F{cyan}>=>%f"
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

# source all the profiles in ~/.nix-profile/etc/profile.d/
if [ -d "$HOME/.nix-profile/etc/profile.d" ]
then
  for i in $HOME/.nix-profile/etc/profile.d/*.sh; do
    if [ -r "$i" ]; then
      . "$i"

    fi
  done
fi

[[ -s "$XDG_CONFIG_HOME/shell/index.sh"  ]] && source "$XDG_CONFIG_HOME/shell/index.sh"

[[ -s  "$XDG_CONFIG_HOME/broot/launcher/bash/br" ]] && source "$XDG_CONFIG_HOME/broot/launcher/bash/br"

# init zsh hooks
command -v thefuck > /dev/null \
  && eval $(thefuck --alias) \
  || echo "thefuck not installed"

command -v direnv > /dev/null \
  && eval "$(direnv hook zsh)" \
  || echo "direnv not installed"

command -v zoxide > /dev/null \
  && eval "$(zoxide init zsh)" \
  || echo "zoxide not installed"

# Define an init function and append to zvm_after_init_commands
# this is required to work around zsh-vi-mode plugin
function my_init() {
  if [[ $OSNAME != 'Darwin' ]]; then
    source /usr/share/fzf/completion.zsh
    source /usr/share/fzf/key-bindings.zsh
  fi
}
my_init

autoload -U compinit && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
