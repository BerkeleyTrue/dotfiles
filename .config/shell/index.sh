#!/bin/sh
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

# nvm
