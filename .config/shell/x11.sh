startxmonad() {
  if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep X || startx $HOME/.config/x11/xinitrc >$HOME/.local/share/x11/x11.log 2>&1
  fi
}

startplasma() {
  if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep X || startx $HOME/.config/x11/xinitrc kde >$HOME/.local/share/x11/x11.log 2>&1
  fi
}
