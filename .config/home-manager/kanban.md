## To Do

- feat: move zsh config to nix
  > this should allow easier config of other programs
- feat: install zellij
  > depends on move zsh to nix
- feat: add jdownloader overlay
  > I've seen a few examples of peeps downloading jar files and use a hash of
    that for an overlay

## Doing

- fix: tmux should not quit on exit to TTY
  > when testing xmonad, reentering xmonad triggers a restart of tmux server
    We only want to start if not already running, or start on login?

## Done

- feat: rofi-network-manager should have an .Desktop entry
