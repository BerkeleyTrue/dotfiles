# Run by login shell (zsh)

# If we are in TTY1 (login) and not already running xmonad,
# then run initrc (which should start xmonad within)
if [[ "$(tty)" = "/dev/tty1" ]]; then
  pgrep xmonad || startx $HOME/.config/x11/xinitrc > $HOME/.local/share/x11/x11.log 2>&1
fi
