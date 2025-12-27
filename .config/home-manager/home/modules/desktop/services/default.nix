{...}: {
  imports = [
    # session targets
    ./desktop-targets.nix
    ./shutdown-graphical.nix

    # niri/wayland
    ./niri.nix
    ./wayland.nix

    # desktop services
    ./blueman.nix
    ./flameshot.nix
    ./flatpak.nix
    ./gh-user.nix
    ./kanata.nix
    ./hypridle.nix
    ./nix-collect-garbage.nix
    ./notessync.nix
    ./notification.nix
    # ./pasystray.nix # doesn't work in wayland https://github.com/christophgysin/pasystray/issues/90
    ./security.nix
    # ./sleep.nix # handled by hypridle
    # ./status-notifier-watcher.nix
    ./status-bar.nix
    ./taiscale-systray.nix
    ./task.nix
    ./tmux.nix
    ./xdg.nix
  ];

  # needed to load env vars for systemd user services
  xdg.configFile."systemd/user.conf".text = ''
    [Manager]
    ManagerEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
    DefaultEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
  '';
}
