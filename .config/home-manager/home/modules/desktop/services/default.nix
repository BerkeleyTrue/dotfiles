{...}: {
  imports = [
    # session targets
    ./desktop-targets.nix
    ./shutdown-graphical.nix
    ./niri.nix
    ./wayland.nix

    # desktop services
    ./awww.nix
    ./blueman.nix
    ./flameshot.nix
    ./flatpak.nix
    ./gh-user.nix
    ./hypridle.nix
    ./kanata.nix
    ./mako.nix
    ./nix-collect-garbage.nix
    ./notessync.nix
    ./security.nix
    ./swaync.nix
    ./taiscale-systray.nix
    ./task.nix
    ./tmux.nix
    ./waybar.nix
    ./xdg.nix

    # ./pasystray.nix # doesn't work in wayland https://github.com/christophgysin/pasystray/issues/90
    # ./sleep.nix # handled by hypridle
    # ./status-notifier-watcher.nix # handled by waybar itself
  ];

  # needed to load env vars for systemd user services
  xdg.configFile."systemd/user.conf".text = ''
    [Manager]
    ManagerEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
    DefaultEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
  '';
}
