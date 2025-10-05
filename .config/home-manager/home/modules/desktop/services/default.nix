{...}: {
  imports = [
    # xmonad
    ./x11-session.nix 
    ./x11-foundation.nix 
    ./xmonad.nix 
    ./desktop-targets.nix 
    ./shutdown-graphical.nix
    ./taffybar.nix

    # x11 desktop
    ./autorandr
    ./nitrogen.nix 
    ./pasystray.nix
    ./unclutter.nix
    ./xcape.nix
    ./xplugd.nix
    ./picom.nix 

    # niri
    ./niri.nix
    ./wayland.nix

    # wayland desktop

    # desktop services 
    ./notification.nix
    ./blueman.nix
    ./flameshot.nix
    ./flatpak.nix
    ./gh-user.nix
    ./kanata.nix
    ./nix-collect-garbage.nix
    ./notessync.nix
    ./security.nix
    ./sleep.nix # TODO: make wayland compatible
    ./status-notifier-watcher.nix # is this x11 specific?
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
