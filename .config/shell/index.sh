#! /bin/bash
isosx="$(uname | grep -q Darwin)"
config="$HOME/.config/shell"
TERMINAL="hyper"

[[ -s "$HOME/.private_aliases"  ]] && source "$HOME/.private_aliases"
[[ -s "$config/common_paths.sh"  ]] && source "$config/common_paths.sh"
[[ -s "$config/common_aliases.sh"  ]] && source "$config/common_aliases.sh"
[[ -s "$config/common_helpers.sh"  ]] && source "$config/common_helpers.sh"
[[ -s "$config/npm-helpers.sh"  ]] && source "$config/npm-helpers.sh"
[[ -s "$config/git.sh" ]] && source "$config/git.sh"
[[ isosx ]] && [[ -s "./osx.sh" ]] && source "$config/osx.sh"
