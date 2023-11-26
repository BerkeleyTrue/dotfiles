{...}: {
  imports = [
    ./autorandr
    ./blueman.nix
    ./dunst.nix
    ./flameshot.nix
    ./nitrogen.nix
    ./nix-collect-garbage.nix
    ./notessync.nix
    ./pasystray.nix
    ./picom.nix
    ./security.nix
    ./sleep.nix
    ./status-notifier-watcher.nix
    ./task.nix
    ./tmux.nix
    ./tray.nix
    ./unclutter.nix
    ./xcape.nix
    ./xdg.nix
    ./xkbmap.nix
    ./xplugd.nix
  ];

  # needed to load env vars for systemd user services
  xdg.configFile."systemd/user.conf".text = ''
    [Manager]
    ManagerEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
    DefaultEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
  '';
}
