{self, ...}: {
  flake.modules.homeManager.desktop = {
    imports = with self.modules.homeManager; [
      blueman-applet
      desktop-apps
      desktop-targets
      flatpak
      hypridle
      hyprlock
      mako
      niri
      polkit
      powermenu
      icedshell
      swaync
      tailscale-systray
      wallpaper
      wayland
      xdg
    ];

    systemd.user.targets.shutdown-graphical = {
      Unit = {
        Description = "Shutdown graphical session";
        DefaultDependencies = "no";
        StopWhenUnneeded = true;

        After = ["graphical-session.target" "graphical-session-pre.target"];
        Conflicts = ["graphical-session.target" "graphical-session-pre.target"];
      };
    };
  };
}
