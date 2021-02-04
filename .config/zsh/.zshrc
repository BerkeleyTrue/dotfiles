# clear right prompt on execute
setopt TRANSIENT_RPROMPT

export DEFAULT_USER=`whoami`
export FZF_DEFAULT_COMMAND='fd .'
export WD_CONFIG="$XDG_CONFIG_HOME/warpdrive/warprc"
export TASKDATA="$XDG_CONFIG_HOME/task"
export TASKRC="$TASKDATA/taskrc"
export TIMEWARRIORDB="$XDG_CONFIG_HOME/timewarrior"

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

### begin antigen ###

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

### end antigen ###

[[ -s "$XDG_CONFIG_HOME/shell/index.sh"  ]] && source "$XDG_CONFIG_HOME/shell/index.sh"

[[ -s  "$XDG_CONFIG_HOME/broot/launcher/bash/br" ]] && source "$XDG_CONFIG_HOME/broot/launcher/bash/br"

# Define an init function and append to zvm_after_init_commands
# this is required to work around zsh-vi-mode plugin
function my_init() {
  if [[ $OSNAME != 'Darwin' ]]; then
    source /usr/share/fzf/completion.zsh
    source /usr/share/fzf/key-bindings.zsh
  fi
}

zvm_after_init_commands+=(my_init)

autoload -U compinit && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
