# Zsh configuration
# runs on every new shell

# clear right prompt on execute
setopt TRANSIENT_RPROMPT

path+=($HOME/.local/bin) # local binaries and scripts
path+=($NIX_PROFILE/bin) # nix binaries
path+=($GOPATH/bin) # go binaries

TERMINAL="kitty"
DEFAULT_USER=`whoami`

# Zsh config
ZSH="$XDG_CONFIG_HOME/zsh"
ZSH_CACHE_DIR="$HOME/.cache/zsh"
HISTFILE="$XDG_DATA_HOME/zsh/.zsh_history"

mkdir -p "$XDG_DATA_HOME/zsh"

# Antigen configs
ADOTDIR="$XDG_CONFIG_HOME/antigen"
ANTIGEN_LOG="$HOME/.cache/antigen/antigen.log"
# uncomment to debug antigen
# export ANTIGEN_LOG=$ANTIGEN_LOG
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
if [[ $(uname) = 'Darwin' ]]; then

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
fi

# check if antigen is installed
if [ -e "$NIX_PROFILE/share/antigen/antigen.zsh" ]; then
  source $NIX_PROFILE/share/antigen/antigen.zsh
else
  echo "antigen not found"
fi

# source theme
source $ZSH/ghanima.zsh

antigen use oh-my-zsh
antigen bundle djui/alias-tips
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle docker
antigen bundle taskwarrior
antigen bundle systemd
antigen apply

zle -N zle-line-finish ghanima::hooks::line-finish

zvm_after_select_vi_mode_commands+=(ghanima::hooks::zle-keymap-select)
ZVM_VI_HIGHLIGHT_FOREGROUND=black
ZVM_VI_HIGHLIGHT_BACKGROUND=yellow
ZVM_LINE_INIT_MODE='i'

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
command -v direnv > /dev/null \
  && eval "$(direnv hook zsh)" \
  || echo "direnv not installed"

command -v zoxide > /dev/null \
  && eval "$(zoxide init zsh)" \
  || echo "zoxide not installed"

# Define an init function and append to zvm_after_init_commands
# this is required to work around zsh-vi-mode plugin
function my_init() {
  if [[ $(uname) != 'Darwin' ]]; then
    source $NIX_PROFILE/share/fzf/completion.zsh
    source $NIX_PROFILE/share/fzf/key-bindings.zsh
  fi
}
zvm_after_init_commands+=(my_init)

source "$ZSH/zsh-copy-paste.zsh"
source "$ZSH/nix-packages.zsh"

autoload -U compinit && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
