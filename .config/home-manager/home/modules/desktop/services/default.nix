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

  ];
}
