{...}: {
  imports = [
    ./x11-session.nix 
    ./x11-foundation.nix 
    ./xmonad.nix 
    ./desktop-targets.nix 
    ./shutdown-graphical.nix

    # desktop services 
    ./autorandr
    ./blueman.nix
    ./dunst.nix 
    ./flameshot.nix
    ./flatpak.nix
    ./gh-user.nix
    ./kanata.nix
    ./nitrogen.nix 
    ./nix-collect-garbage.nix
    ./notessync.nix
    ./pasystray.nix
    ./picom.nix 
    ./security.nix
    ./sleep.nix
    ./status-notifier-watcher.nix
    ./taffybar.nix
    ./taiscale-systray.nix
    ./task.nix
    ./tmux.nix
    ./unclutter.nix
    ./xcape.nix
    ./xdg.nix
    ./xplugd.nix
  ];

  # needed to load env vars for systemd user services
  xdg.configFile."systemd/user.conf".text = ''
    [Manager]
    ManagerEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
    DefaultEnvironment="XDG_DATA_DIRS=%h/.nix-profile/share:%h/.local/share:/usr/local/share:/usr/share"
  '';
}
