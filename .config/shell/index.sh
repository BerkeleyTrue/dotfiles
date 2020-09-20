ISOSX="$(uname | grep -q Darwin)"
SHELL_CONF="$HOME/.config/shell"
TERMINAL="alacritty"

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/local/share:/usr/share:$XDG_DATA_HOME}

[[ -s "$HOME/.private_aliases"  ]] && source "$HOME/.private_aliases"

# don't use conditional check for files that should always be there
source $SHELL_CONF/common_paths.sh
source $SHELL_CONF/common_aliases.sh
source $SHELL_CONF/common_helpers.sh
source $SHELL_CONF/npm.sh
source $SHELL_CONF/git.sh

[[ -s "$SHELL_CONF/.mr.sh" ]] && source $SHELL_CONF/.mr.sh

[[ ISOSX ]] && [[ -s "./osx.sh" ]] && source $SHELL_CONF/osx.sh

# nvm
export NVM_DIR=${NVIM_DIR:-$XDG_CONFIG_HOME/nvm}
source $SHELL_CONF/nvm.sh
