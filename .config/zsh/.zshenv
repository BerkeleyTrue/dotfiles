# these are redundant, but useful for clarity
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_DATA_DIRS=$HOME/.nix-profile/share:$HOME/.share:"${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
export XDG_CURRENT_DESKTOP=Unity
export NIX_PROFILE="$HOME/.nix-profile"

export FZF_DEFAULT_COMMAND='fd .'
export TASKDATA="$XDG_CONFIG_HOME/task"
export TASKRC="$TASKDATA/taskrc"
export TIMEWARRIORDB="$XDG_CONFIG_HOME/timewarrior"
export GOPATH=$HOME/dvlpmnt/go
