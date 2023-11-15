LOG=$HOME/.local/share/x11/x11.log

startxmonad() {
  systemctl --user import-environment PATH DBUS_SESSION_BUS_ADDRESS
  if [[ "$(tty)" = "/dev/tty1" ]]; then
    pgrep -x Xorg || startx $HOME/.config/x11/xinitrc &>$LOG
  fi
}

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
