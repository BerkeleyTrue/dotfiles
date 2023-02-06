OSNAME=$(uname)


export GOPATH=$HOME/dvlpmnt/go
export XDG_CURRENT_DESKTOP=Unity

PATH=$PATH:$HOME/.local/bin:$GOPATH/bin:$HOME/.cargo/bin:$HOME/.nix-profile/bin


# source all the profiles in ~/.nix-profile/etc/profile.d/
for i in $HOME/.nix-profile/etc/profile.d/*.sh; do
    if [ -r "$i" ]; then
        . "$i"
    fi
done

# Run by login shell (zsh)

# If we are in TTY1 (login) and not already running xorg,
# then run initrc (which should start xorg within)
# if [[ "$(tty)" = "/dev/tty1" ]]; then
#   pgrep X || startx $HOME/.config/x11/xinitrc > $HOME/.local/share/x11/x11.log 2>&1
# fi

if [[ $OSNAME != 'Darwin' ]]; then
  xcape -e 'Control_L=Escape'
fi

bindkey -a -r ':' # remove execute widget command
