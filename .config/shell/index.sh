#!/bin/bash
# put important envs here
export PROFILE_ALREADY_SOURCED=0

export NIX_PROFILE="$HOME/.nix-profile"

export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

export FZF_DEFAULT_COMMAND='fd .'
export TASKDATA="$XDG_CONFIG_HOME/task"
export TASKRC="$TASKDATA/taskrc"
export TIMEWARRIORDB="$XDG_CONFIG_HOME/timewarrior"
export GOPATH=$HOME/dvlpmnt/go
export XSECURELOCK_NO_COMPOSITE=1 # xsecurelock doesn't work w/ picom

if [[ $ALREADY_SOURCED -eq 0 ]]; then
	export PATH=$HOME/.local/bin:$NIX_PROFILE/bin:$PATH
	export XDG_DATA_DIRS=$HOME/.nix-profile/share:$HOME/.share:$XDG_DATA_DIRS
fi

export PROFILE_ALREADY_SOURCED=1

ISOSX="$(uname | grep -q Darwin)"
SHELL_CONF="$XDG_CONFIG_HOME/shell"
TERMINAL="kitty"

[[ -s "$SHELL_CONF/.private_aliases" ]] && source $SHELL_CONF/.private_aliases

files=(
	'ansible'
	'common_aliases'
	'common_helpers'
	'common_paths'
	'docker'
	'npm'
	'nix'
	'task'
	'git'
	'mr_func'
	'pacman'
	'systemd'
	'x11')

# don't use conditional check for files that should always be there
for file in $files; do
	fname="$SHELL_CONF/$file.sh"
	# debugging
	# echo $fname
	source $fname
done
unset files
unset file
unset fname

[[ -s "$SHELL_CONF/.mr.sh" ]] && source $SHELL_CONF/.mr.sh

[[ ISOSX ]] && [[ -s "./osx.sh" ]] && source $SHELL_CONF/osx.sh
