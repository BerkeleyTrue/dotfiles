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

bindkey -a -r ':' # remove execute widget command
