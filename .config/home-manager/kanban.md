## To Do

## Doing

- Switch to wayland compositor
  - [x] hierarchy x11 
  - [ ] niri wm service and session management
  - [ ] Anyrun or tofi 
  - [ ] waybar https://github.com/Alexays/Waybar
  - [ ] Panels with Astal https://aylur.github.io/astal/
  - [ ] mako notifier
  - [ ] hyprlock https://wiki.hypr.land/Hypr-Ecosystem/hyprlock/
  - [ ] catppuccin cursor https://github.com/catppuccin/cursors
  - [ ] swww for wallpaper https://github.com/LGFae/swww

## Done

- feat: rofi-network-manager should have an .Desktop entry
- fix: tmux should not quit on exit to TTY
  > when testing xmonad, reentering xmonad triggers a restart of tmux server
    We only want to start if not already running, or start on login?
- feat: move zsh config to nix
  > this should allow easier config of other programs
