LOG=$HOME/.local/share/x11/x11.log

startxmonad() {
  if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep X || startx $HOME/.config/x11/xinitrc >$LOG 2>&1
  fi
}

startplasma() {
  if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep X || startx $HOME/.config/x11/xinitrc kde >$LOG 2>&1
  fi
}

x11logtail() {
  tail -f $LOG
}

x11logvim() {
  nvim $LOG
}
