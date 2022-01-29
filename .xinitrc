#!/bin/sh
#
# ~/.xinitrc
# @deprecated

# Executed by startx (run your window manager from here)
# If initx or startx don't find this file, they will run /etc/X11/xinit/xinitrc by default
# not run by lightdm

echo "deprecated: use \$XDG_CONFIG_HOME/x11/xinitrc"

source $XDG_CONFIG_HOME/x11/xinitrc
