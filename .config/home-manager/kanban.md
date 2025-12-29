## To Do

## Doing

- Switch to wayland compositor
  - [x] hierarchy x11 
  - [x] niri wm service and session management
  - [x] Anyrun or tofi 
    - [x] actually stick with rofi. Now supports wayland!
  - [x] waybar https://github.com/Alexays/Waybar
    - [ ] style
    - [ ] configure
    - [ ] add 2nd for rena
    - [ ] add bars for delora
  - [ ] Panels with Astal https://aylur.github.io/astal/
  - [x] mako notifier
    - [x] style
    - [x] notify send (local/bin/makoify)
  - [x] hyprlock https://wiki.hypr.land/Hypr-Ecosystem/hyprlock/
    - [x] style (using default style with catppuccin)
  - [x] catppuccin cursor https://github.com/catppuccin/cursors
  - [x] swww for wallpaper https://github.com/LGFae/swww

## Done

- feat: rofi-network-manager should have an .Desktop entry
- fix: tmux should not quit on exit to TTY
  > when testing xmonad, reentering xmonad triggers a restart of tmux server
    We only want to start if not already running, or start on login?
- feat: move zsh config to nix
  > this should allow easier config of other programs
