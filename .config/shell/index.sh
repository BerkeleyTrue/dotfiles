#! /bin/bash

isosx="$(uname | grep -q Darwin)"
config="$HOME/.config/shell"
TERMINAL="alacritty"

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/local/share:/usr/share:$XDG_DATA_HOME}

[[ -s "$HOME/.private_aliases"  ]] && source "$HOME/.private_aliases"
[[ -s "$config/common_paths.sh"  ]] && source "$config/common_paths.sh"
[[ -s "$config/common_aliases.sh"  ]] && source "$config/common_aliases.sh"
[[ -s "$config/common_helpers.sh"  ]] && source "$config/common_helpers.sh"
[[ -s "$config/npm-helpers.sh"  ]] && source "$config/npm-helpers.sh"
[[ -s "$config/git.sh" ]] && source "$config/git.sh"
[[ -s "$config/.mr.sh" ]] && source "$config/.mr.sh"
[[ isosx ]] && [[ -s "./osx.sh" ]] && source "$config/osx.sh"

# nvm
export NVM_DIR=${NVIM_DIR:-$XDG_CONFIG_HOME/nvm}
source $config/nvm.sh
