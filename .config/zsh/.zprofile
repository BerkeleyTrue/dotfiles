# runs on login shells
# NOTE: tmux by default will run login shells, see https://wiki.archlinux.org/title/tmux#Start_a_non-login_shells

[[ -s "$HOME/.config/shell/index.sh"  ]] && source "$HOME/.config/shell/index.sh" || echo "shell config not found"

# import environment variables from systemd
systemctl --user import-environment PATH

# start tmux server
systemctl --user start tmux-server.service
