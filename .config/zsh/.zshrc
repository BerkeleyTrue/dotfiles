# Zsh configuration
# runs on every new shell
[[ -s "$HOME/.config/shell/index.sh"  ]] && source "$HOME/.config/shell/index.sh" || echo "shell config not found"

mkdir -p "$XDG_DATA_HOME/zsh"

# clear right prompt on execute
setopt TRANSIENT_RPROMPT

bindkey -a -r ':' # remove execute widget command


TERMINAL="kitty"
DEFAULT_USER=`whoami`

# Zsh config
ZSH="$XDG_CONFIG_HOME/zsh"
ZSH_CACHE_DIR="$HOME/.cache/zsh"

# history config
HISTFILE="$XDG_DATA_HOME/zsh/.zsh_history"
HISTSIZE=100000 # number of commands to keep in memory
SAVEHIST=100 # number of commands to save to history file


setopt MULTIOS              # enable redirect to multiple streams: echo >file1 >file2
setopt LONG_LIST_JOBS       # show long list format job notifications
setopt INTERACTIVECOMMENTS  # recognize comments

setopt EXTENDED_HISTORY       # record unix timestamp of command in HISTFILE | <timestamp>:<elapsed seconds>;command
setopt HIST_EXPIRE_DUPS_FIRST # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt HIST_IGNORE_ALL_DUPS   # delete old recorded command if new command is a duplicate
setopt HIST_IGNORE_SPACE      # ignore commands that start with space
setopt HIST_VERIFY            # show command with history expansion to user before running it
setopt SHARE_HISTORY          # share command history data

unsetopt MENU_COMPLETE   # do not autoselect the first completion entry
unsetopt FLOWCONTROL

setopt AUTO_MENU         # show completion menu on successive tab press
setopt COMPLETE_IN_WORD
setopt ALWAYS_TO_END

# completion config
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

# Antigen configs
ADOTDIR="$XDG_CONFIG_HOME/antigen"
ANTIGEN_LOG="$HOME/.cache/antigen/antigen.log"
# uncomment to debug antigen
# export ANTIGEN_LOG=$ANTIGEN_LOG
ZSH_THEME="agnoster"

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

# init zsh hooks
command -v direnv > /dev/null \
  && eval "$(direnv hook zsh)" \
  || echo "direnv not installed"

command -v zoxide > /dev/null \
  && eval "$(zoxide init zsh)" \
  || echo "zoxide not installed"

command -v atuin > /dev/null \
  && eval "$(atuin init zsh --disable-up-arrow)" \
  || echo "atuin not installed"


# Define an init function and append to zvm_after_init_commands
# this is required to work around zsh-vi-mode plugin
function my_init() {
  if [[ $(uname) != 'Darwin' ]]; then
    source $NIX_PROFILE/share/fzf/completion.zsh
    # source $NIX_PROFILE/share/fzf/key-bindings.zsh # removed for atuin search
  fi

  # custom zsh completions
  source $XDG_CONFIG_HOME/shell/comp.zsh
  bindkey -M vicmd '^R' atuin-search-vicmd
  bindkey -M viins '^R' atuin-search-viins
}
zvm_after_init_commands+=(my_init)

source "$ZSH/zsh-copy-paste.zsh"
source "$ZSH/nix-packages.zsh"

autoload -U compinit && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION

# run random-command on shell start if it exists
command -v random-clj-doc > /dev/null && random-clj-doc
