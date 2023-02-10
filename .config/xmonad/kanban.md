## To Do


## Doing

- move to taffybar
  * [x] wakatime
  * [x] current app title
  * [x] eth info
  * [x] weather
  * [x] current layout
  * [x] picom switch
  * [x] power menu button
  * [x] hdd space
  * [ ] add second bar
  * [ ] add switch for laptop

## Done

- move to src dir
  > It's pretty annoying that source files are in the project root. These should be in a src dir
  * [x] update cabal file
  * [x] update haskell build file
  * [x] update stack build
- move to xdg_conf_home
  > These files should live in dvl/haskell dir
- add xmobar compile script
- clean up hooks
  > hooks are all in different formats. These should be a little more uniform
- add dynamic scratpad
- seperate by CCP
  * [ ] mouse bindings
  * [ ] manage hooks
- fix picom transparency
  > No idea why this isn't working
- move to standalone tray
  > Trayer is annoying to configure and ignores struct toggling
  > UPDATE: can't get stalone tray to even appear. Lame
- incorporate systemd
  * [x] Install xplugd and rerun xkbmap
  * [x] add tmux server service
    * [x] needs to start as part of graphical session pre so clipboard works as
        expected
- move from trayer to taffybar sni tray
  > Will wait to try again in a while. Doesn't seem to work with all systray apps
