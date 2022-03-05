#!/bin/sh

set -e

stack build :xmonad --verbosity error
stack install :xmonad
cd xmobar-git
stack install --flag xmobar:all_extensions
