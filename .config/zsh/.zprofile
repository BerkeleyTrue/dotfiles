export GOPATH=$HOME/dvlpmnt/go

PATH=$PATH:$HOME/.local/bin:$GOPATH/bin:$HOME/.cargo/bin
# Run by login shell (zsh)

# If we are in TTY1 (login) and not already running xorg,
# then run initrc (which should start xorg within)
# if [[ "$(tty)" = "/dev/tty1" ]]; then
#   pgrep X || startx $HOME/.config/x11/xinitrc > $HOME/.local/share/x11/x11.log 2>&1
# fi
xcape -e 'Control_L=Escape'
bindkey -a -r ':' # remove execute widget command
