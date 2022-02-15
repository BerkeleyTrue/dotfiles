source $HOME/.config/user-dirs.dirs
# Run by login shell (zsh)

mkdir -p "$XDG_CONFIG_DATA/share/x11"
# If we are in TTY1 (login) and not already running xmonad,
# then run initrc (which should start xmonad within)
if [[ "$(tty)" = "/dev/tty1" ]]; then
  pgrep xmonad || startx "$XDG_CONFIG_HOME/x11/xinitrc" > "$XDG_CONFIG_DATA/share/x11/x11.log" 2>&1
fi
