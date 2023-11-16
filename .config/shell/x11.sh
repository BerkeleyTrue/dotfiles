LOG=$HOME/.local/share/x11/x11.log

startplasma() {
  if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep -x Xorg || startx $HOME/.config/x11/xinitrc kde &>$LOG
  fi
}

x11logtail() {
  tail -f $LOG
}

x11logvim() {
  nvim $LOG
}
