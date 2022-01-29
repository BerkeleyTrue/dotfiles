# Run by login shell (zsh)

# If we are in TTY1 (login) and not already running xmonad,
# then run initrc (which should start xmonad within)
if [[ "$(tty)" = "/dev/tty1" ]]; then
  pgrep xmonad || startx "$XDG_CONFIG_HOME/x11/xinitrc"
fi
