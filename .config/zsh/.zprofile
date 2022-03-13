# Run by login shell (zsh)

# If we are in TTY1 (login) and not already running xorg,
# then run initrc (which should start xorg within)
if [[ "$(tty)" = "/dev/tty1" ]]; then
  pgrep X || startx $HOME/.config/x11/xinitrc > $HOME/.local/share/x11/x11.log 2>&1
fi
